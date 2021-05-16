{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Lib where

import Control.Monad.Trans.Class (lift)
import Control.Concurrent.MVar (MVar)
import Control.Monad.Trans.State.Lazy (StateT)
import Data.Functor ((<&>))
import Data.Function (on)
import Data.Foldable (toList)
import Data.Bifunctor (bimap, second, first)
import Data.ByteString.Lazy (ByteString)
import Data.Either (either)
import Data.List.Extra (unescapeHTML)
import Data.List.NonEmpty (NonEmpty)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes, fromMaybe, fromJust, mapMaybe, listToMaybe)
import Data.List hiding (groupBy)
import Data.Char (chr, ord, isSpace)
import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.IO (hPutStr)
import System.IO.Temp
  ( withSystemTempDirectory
  , withSystemTempFile
  , withTempFile
  , withTempDirectory
  , getCanonicalTemporaryDirectory)


import qualified Control.Concurrent.MVar as MVar
import qualified Control.Monad.Trans.State.Lazy as State
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text (hPutStr)
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Hoogle
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http (tlsManagerSettings)
import qualified Options.Applicative as O
import qualified System.Console.Haskeline as CLI
import qualified System.Process as Process
import qualified Text.HTML.DOM as HTML
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Text.PrettyPrint.ANSI.Leijen.Internal as P
import qualified Text.XML as XML

data Options = Options
  { count :: Int
  , pageSize :: Int
  }

type TargetGroup = NonEmpty Hoogle.Target

data ShellState = ShellState
  { sLastResults :: [TargetGroup]
  , sLastShown :: Int
  , sManager :: Http.Manager
  , sCache :: Map Url (MVar ByteString)
  , sOptions :: Options
  }

type M a = StateT ShellState (CLI.InputT IO) a

defaultPageSize = 100

cliOptions :: O.ParserInfo Options
cliOptions = O.info parser $ O.header " \
  \Hoogle CLI is a command line interface to Hoogle.\
  \Hoogle is a Haskell API search engine, which allows you to search the Haskell libraries on Stackage by either function name, or by approximate type signature."
  where
    parser = Options
      <$> O.option O.auto
          (O.long "count"
          <> O.metavar "INT"
          <> O.help "String to search for"
          <> O.value 20)
      <*> O.option O.auto
          (O.long "page-size"
          <> O.metavar "INT"
          <> O.help "How many results to show per page"
          <> O.value defaultPageSize)

runSearch :: String -> M [Hoogle.Target]
runSearch term = do
  Options{..} <- State.gets sOptions
  req <- Http.parseRequest "https://hoogle.haskell.org"
    <&> Http.setQueryString
      [ ("mode", Just "json")
      , ("start", Just "1")
      , ("hoogle", Just $ Text.encodeUtf8 $ Text.pack term)
      ]
  res <- fetch req
  either error return $ Aeson.eitherDecode res

toGroups :: [Hoogle.Target] -> [TargetGroup]
toGroups
  = mapMaybe NonEmpty.nonEmpty
  . groupBy relevantFields
  where
    relevantFields target = target
      { Hoogle.targetURL = ""
      , Hoogle.targetPackage = Nothing
      , Hoogle.targetModule = Nothing
      }

groupBy :: Ord b => (a -> b) -> [a] -> [[a]]
groupBy f vs = go mempty vs
  where
    go res []
      = map reverse
      $ reverse
      $ fst
      $ foldr takeOnce ([], res)
      $ reverse vs
    go res (x:xs) = go newRes xs
      where newRes = Map.insertWith (++) (f x) [x] res

    takeOnce x (out, m) =
      let key = f x in
      case Map.lookup key m of
        Nothing -> (out, m)
        Just v -> (v:out, Map.delete key m)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x xs = y : splitOn x (drop 1 ys)
  where
    (y, ys) = break (== x) xs

someFunc :: IO ()
someFunc = do
  options <- O.execParser cliOptions
  manager <- Http.newManager Http.tlsManagerSettings
  CLI.runInputT CLI.defaultSettings
    $ State.evalStateT loop ShellState
        { sLastResults = []
        , sLastShown = 0
        , sManager = manager
        , sOptions = options
        , sCache = mempty
        }

data Cmd
  = Search String
  | ViewDocs Int
  | ViewSource Int
  | ViewModuleDocs Int
  | Quit

commands :: [String]
commands = [ "src", "quit", "mdoc" ]

fillPrefix :: String -> Maybe String
fillPrefix v = find (v `isPrefixOf`) commands

parseCommand :: String -> Either String Cmd
parseCommand str = case str of
  (':':xs) -> do
    let (mcmd, args) = bimap fillPrefix tail $ break isSpace xs
    cmd <- maybe (Left "Unknown command") Right mcmd
    let intCmd f
          | Just n <- readMaybe args = Right (f n)
          | otherwise = Left $
            "Command :" <> cmd <> "expects an integer argument"
    case cmd of
        "src" -> intCmd ViewSource
        "mdoc" -> intCmd ViewModuleDocs
        "quit" -> Right Quit
  x | Just n <- readMaybe x -> Right (ViewDocs n)
  _ -> Right $ Search str

loop :: M ()
loop = do
  minput <- lift $ CLI.getInputLine "hoogle> "
  case parseCommand $ fromMaybe "" minput of
    Left err -> liftIO (putStrLn err) >> loop
    Right Quit -> return ()
    Right cmd -> runCommand cmd >> loop

runCommand :: Cmd -> M ()
runCommand = \case
  Quit -> return ()
  Search str -> do
    options <- State.gets sOptions
    res <- toGroups <$> runSearch str
    liftIO $ do
      P.putDoc
        $ P.vsep
        $ reverse
        $ numbered
        $ take (pageSize options)
        $ map viewCompact res
      putStrLn ""
    State.modify' $ \s -> s
        { sLastResults = res
        , sLastShown = pageSize options
        }
  ViewDocs ix -> do
    tgroup <- getTargetGroup ix
    liftIO $ do
      P.putDoc $ viewFull tgroup
      putStrLn ""
  ViewModuleDocs ix -> do
    tgroup <- getTargetGroup ix
    target <- promptSelectOne tgroup
    html <- fetch' (moduleLink target)
    -- let docs = toModuleDocs html
    -- width of 60 + 60
    liftIO $ P.putDoc $ viewModule html

  ViewSource ix -> do
    tgroup <- getTargetGroup ix
    target <- promptSelectOne tgroup
    editSource target

getTargetGroup :: Int -> M TargetGroup
getTargetGroup ix = do
    results <- State.gets sLastResults
    return $ results !! (ix - 1)

promptSelectOne :: TargetGroup -> M Hoogle.Target
promptSelectOne tgroup =
  case toList tgroup of
    [target] -> return target
    targets -> do
      liftIO $ do
        putStrLn "Select one:"
        P.putDoc
          $ P.vsep
          $ reverse
          $ numbered
          $ mapMaybe viewPackageAndModule targets
        putStrLn ""

      num <- lift $ CLI.getInputLine ": "
      case readMaybe =<< num of
        Just n -> case listToMaybe $ drop (n - 1) targets of
          Just target -> return target
          Nothing -> do
            liftIO $ putStrLn "Invalid index"
            promptSelectOne tgroup
        Nothing -> do
          liftIO $ putStrLn "Number not recognised"
          promptSelectOne tgroup

editSource :: Hoogle.Target -> M ()
editSource target = do
  srcLink <- sourceLink target
  html <- fetch' srcLink
  view (fileInfo srcLink html)

view :: FileInfo -> M ()
view (FileInfo filename mline content) = do
  let line = maybe "" (("+" <>) . show) mline
  liftIO $ withSystemTempFile filename $ \fullpath handle -> do
    Text.hPutStr handle content
    Process.callCommand $ unwords ["nvim ", fullpath, line]

data ModuleDocs = ModuleDocs
  { mTitle :: Text
  , mContent :: [ModuleItem]
  }

type ModuleItem = XML.Element

toModuleDocs :: HTML -> ModuleDocs
toModuleDocs (HTML src) = head $ do
  let root = XML.documentRoot (HTML.parseLBS src)
  body    <- findM (is "body" . tag) $ children root
  content <- findM (is "content" . id_) $ children body
  let mheader = findM (is "module-header" . id_) (children content)
      mdescription = findM (is "description" . id_) (children content)
  interface <- findM (is "interface" . id_) (children content)
  return ModuleDocs
    { mTitle = maybe "" innerText mheader
    , mContent = mdescription ++ children interface
    }

withTempPath :: String -> (String -> IO a) -> IO a
withTempPath path f = do
  root <- getCanonicalTemporaryDirectory
  go root $ filter (not . null) $ splitOn '/' path
  where
    go parent [] = f parent
    go parent (x:xs) =
      withTempDirectory parent x $ \p -> go p xs

class HasUrl a where
  getUrl :: a -> Url
instance HasUrl ModuleLink where getUrl (ModuleLink url _) = url
instance HasUrl SourceLink where getUrl (SourceLink url _) = url

fetch' :: HasUrl a => a -> M HTML
fetch' x = do
  req <- liftIO $ Http.parseRequest $ getUrl x
  src <- fetch req
  return (HTML src)

fetch :: Http.Request -> M LB.ByteString
fetch req = do
  cache <- State.gets sCache
  let key = show req
  case Map.lookup key cache of
    Just mvar -> liftIO $ MVar.readMVar mvar
    Nothing -> do
      manager <- State.gets sManager
      mvar <- liftIO MVar.newEmptyMVar
      State.modify $ \s -> s { sCache = Map.insert key mvar (sCache s) }
      liftIO $ do
        res <- Http.responseBody <$> Http.httpLbs req manager
        MVar.putMVar mvar res
        return res

-- ================================
-- Pretty printing
-- ================================

viewItem :: Hoogle.Target -> P.Doc
viewItem = prettyHTML . wrapHTML . Hoogle.targetItem

viewCompact :: TargetGroup -> P.Doc
viewCompact tgroup = P.vsep
  [ viewItem $ NonEmpty.head tgroup
  , viewPackageInfoList tgroup
  ]

viewPackageInfoList :: TargetGroup -> P.Doc
viewPackageInfoList
  = P.vsep
  . P.punctuate P.comma
  . mapMaybe viewPackageAndModule
  . toList

viewPackageAndModule :: Hoogle.Target -> Maybe P.Doc
viewPackageAndModule target = do
  pkg <- viewPackage target
  mod <- viewModule target
  return $ pkg P.<+> mod
  where
    viewModule target = do
      mod <- fst <$> Hoogle.targetModule target
      return $ P.magenta (P.text mod)

    viewPackage target = do
      pkg <- fst <$> Hoogle.targetPackage target
      return $ P.black (P.text pkg)

viewModule :: HTML -> P.Doc
viewModule (HTML html) = mconcat $ do
  let root = XML.documentRoot (HTML.parseLBS html)
  body        <- findM (is "body" . tag) $ children root
  content     <- findM (is "content" . id_) $ children body
  let description = findM (is "description" . id_) $ children content
      interface   = findM (is "interface" . id_) $ children content
  prettyHTML <$> (description ++ interface)

-- | Render Haddock's HTML
prettyHTML :: XML.Element -> P.Doc
prettyHTML = unXMLElement
  where
    unXMLElement e = style e $ foldMap unXMLNode $ XML.elementNodes e
    unXMLNode = \case
      XML.NodeInstruction _ -> mempty
      XML.NodeContent txt -> P.vsep
        $ map (foldr (P.<+>) mempty . fmap P.text . words)
        $ lines
        $ unescapeHTML
        $ Text.unpack txt
      XML.NodeComment _ -> mempty
      XML.NodeElement e -> unXMLElement e

    style e = tagStyle e . classStyle e

    classStyle e = case class_ e of
      ""                  -> id
      -- layout
      "doc"               -> P.indent 2
      "subs methods"      -> P.indent 2
      "subs instances"    -> P.indent 2
      "subs constructors" -> P.indent 2
      "inst-left"         -> P.fillBreak 50
      "top"               -> mappend P.linebreak
      -- style
      "caption"           -> P.bold
      "name"              -> P.dullgreen
      "def"               -> P.bold
      "fixity"            -> P.black
      -- invisible
      "link"              -> hide
      "selflink"          -> hide
      -- modify
      "module-header"     -> const $ mconcat $ map unXMLElement $ findM (is "caption" . class_) (children e)
      _                   -> id

    tagStyle e = case tag e of
       "h1"      -> mappend (P.text "# ")
       "h2"      -> mappend (P.text "## ")
       "h3"      -> mappend (P.text "### ")
       "h4"      -> mappend (P.text "#### ")
       "h5"      -> mappend (P.text "##### ")
       "h6"      -> mappend (P.text "###### ")
       "tt"      -> P.green
       "pre"     -> const $ P.indent 2 . P.black . linebreak . P.string . Text.unpack $ innerText e
       "code"    -> P.black
       "a"       -> P.cyan
       "b"       -> P.bold
       "p"       -> linebreak
       "dt"      -> P.bold . linebreak
       "dd"      -> linebreak
       "ol"      -> const $ linebreak $ P.vsep $ numbered $ map unXMLElement (children e)
       "ul"      -> const $ P.vsep $ map (bullet . unXMLElement) (children e)
       -- don't show instance details
       "details" -> hide
       _         -> id

    linebreak doc = P.linebreak <> doc <> P.linebreak
    italics = P.Italicize True
    hide = const mempty

viewFull :: TargetGroup -> P.Doc
viewFull tgroup = P.vsep
  [ divider
  , content
  , divider
  ]
  where
    divider = P.black $ P.text $ replicate 50 '='
    representative = NonEmpty.head tgroup
    content = P.vsep $
      [ viewItem representative
      , viewPackageInfoList tgroup
      , prettyHTML $ wrapHTML $ Hoogle.targetDocs representative
      ] ++ (P.cyan . P.text . Hoogle.targetURL <$> toList tgroup)

wrapHTML :: String -> XML.Element
wrapHTML
  = XML.documentRoot
  . HTML.parseLBS
  . LB.fromStrict
  . Text.encodeUtf8
  . Text.pack
  . (\v -> "<div>" <> v <> "</div>")

unHTML :: String -> String
unHTML = unescapeHTML . removeTags False
  where
    -- remove html tags
    removeTags _ [] = []
    removeTags False ('<':xs) = removeTags True xs
    removeTags True  ('>':xs) = removeTags False xs
    removeTags False (x:xs)   = x:removeTags False xs
    removeTags True  (x:xs)   = removeTags True xs

numbered :: [P.Doc] -> [P.Doc]
numbered = zipWith f [1..]
  where
    f n s = P.fill 2 (P.blue $ P.int n) P.<+> P.align s

bullet :: P.Doc -> P.Doc
bullet doc = P.fill 2 (P.char '•') P.<+> P.align doc

-- ================================
-- Haddock handling
-- ================================

type Url = String

-- | Link to an item in a module page
data ModuleLink = ModuleLink Url Anchor
  deriving (Show)

-- | Link to an item in a src page
data SourceLink = SourceLink Url Anchor
  deriving (Show)

type FileName = String

type FileContent = Text

type Anchor = Text

newtype RelativeUrl = RelativeUrl Text

newtype HTML = HTML ByteString

data FileInfo = FileInfo
  { fName :: FileName
  , fLine :: Maybe Int
  , fContent :: FileContent
  }

-- | Convert an html page into a src file and inform of line
-- number of SourceLink
fileInfo :: SourceLink -> HTML -> FileInfo
fileInfo (SourceLink _ anchor) (HTML doc) = head $ do
  let page = HTML.parseLBS doc
      root = XML.documentRoot page
  head_ <- filter (is "head" . tag) $ children root
  title <- filter (is "title" . tag) $ children head_
  let filename = Text.unpack $ Text.replace "/" "." $ innerText title
  body <- filter (is "body" . tag) $ children root
  return $ FileInfo filename (anchorLine anchor body) (innerText body)

-- | File line where the tag is
anchorLine :: Anchor -> XML.Element -> Maybe Int
anchorLine anchor
  = either Just (const Nothing)
  . anchorNodes 0
  . XML.elementNodes
  where
    anchorNodes :: Int -> [XML.Node] -> Either Int Int
    anchorNodes n = foldM anchorNode n

    anchorNode :: Int -> XML.Node -> Either Int Int -- anchor line or total lines
    anchorNode n = \case
      XML.NodeInstruction _ -> Right n
      XML.NodeContent txt -> Right $ n + Text.count "\n" txt
      XML.NodeComment _ -> Right n
      XML.NodeElement e ->
        if attr "name" e == anchor
          then Left n
          else anchorNodes n (XML.elementNodes e)

-- | Get URL for source file for a target
sourceLink :: Hoogle.Target -> M SourceLink
sourceLink target = do
  let mlink@(ModuleLink _ anchor) = moduleLink target
  html <- fetch' mlink
  let links = sourceLinks mlink html
  case lookup anchor links of
    Nothing -> error $ unlines $
      [ "anchor missing in module docs"
      , show mlink
      ] ++ map show links
    Just link -> return link

-- | Get URL for module documentation
moduleLink :: Hoogle.Target -> ModuleLink
moduleLink target = ModuleLink (dropAnchor url) (takeAnchor url)
  where
    url = Hoogle.targetURL target

takeAnchor :: Url -> Anchor
takeAnchor = Text.pack . tail . dropWhile (/= '#')

dropAnchor :: Url -> Url
dropAnchor = takeWhile (/= '#')

sourceLinks :: ModuleLink -> HTML -> [(Anchor, SourceLink)]
sourceLinks (ModuleLink moduleLink _) (HTML html) = do
  let root = XML.documentRoot (HTML.parseLBS html)
  body        <- filter (is "body" . tag) $ children root
  content     <- filter (is "content" . id_) $ children body
  interface   <- filter (is "interface" . id_) $ children content
  declaration <- filter (is "top" . class_) $ children interface

  signature  <- findM (is "src" . class_) $ children declaration
  url <- map (toSourceUrl . attr "href")
    . findM (is "Source" . innerText)
    $ children signature
  let surl =  SourceLink (dropAnchor url) (takeAnchor url)

  let constructors = filter (is "subs constructors" . class_) $ children declaration
  anchor <- foldMap anchors (signature : constructors)

  return (anchor, surl)
  where
    anchors el = f $ foldMap anchors (children el)
      where
        f = if isAnchor el then (id_ el :) else id

    isAnchor el =
      class_ el == "def" &&
      (Text.isPrefixOf "t:" (id_ el) || Text.isPrefixOf "v:" (id_ el))

    parent = reverse . tail . dropWhile (/= '/') . reverse

    toSourceUrl relativeUrl = parent moduleLink <> "/" <> Text.unpack relativeUrl

findM :: (MonadFail m, Foldable t) => (a -> Bool) -> t a -> m a
findM f x = do
  Just a <- return $ find f x
  return a

is :: Eq a => a -> a -> Bool
is = (==)

children :: XML.Element -> [XML.Element]
children element =
  [ n | XML.NodeElement n <- XML.elementNodes element ]

tag :: XML.Element -> Text
tag = XML.nameLocalName . XML.elementName

id_ :: XML.Element -> Text
id_ = attr "id"

class_ :: XML.Element -> Text
class_ = attr "class"

attr :: Text -> XML.Element -> Text
attr name =
  fromMaybe ""
  . Map.lookup (XML.Name name Nothing Nothing)
  . XML.elementAttributes

innerText :: XML.Element -> Text
innerText el = flip foldMap (XML.elementNodes el) $ \case
  XML.NodeElement e -> innerText e
  XML.NodeInstruction _ -> mempty
  XML.NodeContent txt -> txt
  XML.NodeComment _ -> mempty

foldElement :: Monoid m => (XML.Element -> m) -> XML.Element -> m
foldElement f el = foldr (mappend . foldElement f) (f el) (children el)


