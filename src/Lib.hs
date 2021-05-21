{-# LANGUAGE LambdaCase #-}
module Lib where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.MVar (MVar)
import Control.Monad.Trans.State.Lazy (StateT)
import Data.Functor (void, (<&>))
import Data.Foldable (toList, fold)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (ByteString)
import Data.List.Extra (unescapeHTML)
import Data.List.NonEmpty (NonEmpty)
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.List hiding (groupBy)
import Data.Char (isSpace)
import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.Environment (getEnv)
import System.IO (stdout, Handle)
import System.IO.Temp
  ( withSystemTempFile
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
import qualified System.Console.Terminal.Size as Terminal

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

defaultPageSize :: Int
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

someFunc' :: IO ()
someFunc' = do
  options <- O.execParser cliOptions
  manager <- Http.newManager Http.tlsManagerSettings
  CLI.runInputT CLI.defaultSettings
    $ State.evalStateT
      (do
        runCommand (Search "Data.Set")
        runCommand (ViewDocs 1)
      )
      ShellState
        { sLastResults = []
        , sLastShown = 0
        , sManager = manager
        , sOptions = options
        , sCache = mempty
        }

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
  -- ^ docs provided in the hoogle response
  | ViewExtendedDocs Int
  -- ^ declaration's docs available in the haddock page
  | ViewModuleDocs Int
  -- ^ full haddock for module
  | ViewSource Int
  | Quit

commands :: [String]
commands = [ "src", "quit", "mdoc", "edoc" ]

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
        "edoc" -> intCmd ViewExtendedDocs
        "quit" -> Right Quit
        _ -> error $ "Unknown command: " <> cmd
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
    viewInTerminal
        $ P.vsep
        $ reverse
        $ numbered
        $ take (pageSize options)
        $ map viewCompact res
    State.modify' $ \s -> s
        { sLastResults = res
        , sLastShown = pageSize options
        }
  ViewDocs ix -> do
    tgroup <- getTargetGroup ix
    viewInTerminal $ viewFull tgroup
  ViewExtendedDocs ix -> do
    target <- NonEmpty.head <$> getTargetGroup ix
    let ModuleLink url manchor = moduleLink target
    html <- fetch' url
    let modl = parseModuleDocs html
        desc = case manchor of
          Nothing -> prettyHTML <$> mDescription modl
          Just an -> prettyDecl <$> lookupDecl an modl
    viewInTerminal $ fromMaybe mempty desc
  ViewModuleDocs ix -> do
    tgroup <- getTargetGroup ix
    target <- promptSelectOne tgroup
    html <- fetch' (moduleLink target)
    viewInEditor $ prettyModule $ parseModuleDocs html
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

viewInTerminal :: MonadIO m => P.Doc -> m ()
viewInTerminal = printDoc stdout

viewInEditor :: MonadIO m => P.Doc -> m ()
viewInEditor doc = liftIO $ do
  withSystemTempFile "doc" $ \fullpath handle -> do
    printDoc handle $ P.plain doc
    editor <- getEditor
    void $ Process.system (editor ++ " " ++ fullpath)

printDoc :: MonadIO m => Handle -> P.Doc -> m ()
printDoc handle doc = liftIO $ do
  width <- maybe 80 Terminal.width <$> Terminal.size
  P.displayIO handle $ P.renderSmart 1 width doc
  putStrLn ""

editSource :: Hoogle.Target -> M ()
editSource target = do
  mlink <- sourceLink target
  case mlink of
    Nothing -> liftIO $ putStrLn "no source for file"
    Just link -> do
      html <- fetch' link
      view (fileInfo link html)

view :: FileInfo -> M ()
view (FileInfo filename mline content) = do
  let line = maybe "" (("+" <>) . show) mline
  liftIO $ do
    editor <- getEditor
    withSystemTempFile filename $ \fullpath handle -> do
      Text.hPutStr handle content
      Process.callCommand $ unwords [editor, fullpath, line]

getEditor :: IO String
getEditor = getEnv "VISUAL" <|> getEnv "EDITOR" <|> defaultEditor
  where
    defaultEditor = error "no editor selected"

data ModuleDocs = ModuleDocs
  { mTitle :: String
  , mDescription :: Maybe XML.Element
  , mDeclarations :: [DeclarationDocs]
  }

data DeclarationDocs = DeclarationDocs
  { dAnchors :: Set Anchor
  , dSignature :: XML.Element
  , dContent :: [XML.Element]
  }

type ModuleItem = XML.Element

parseModuleDocs :: HTML -> ModuleDocs
parseModuleDocs (HTML src) = head $ do
  let root = XML.documentRoot (HTML.parseLBS src)
  body    <- findM (is "body" . tag) $ children root
  content <- findM (is "content" . id_) $ children body
  let mtitle = do
        h <- findM (is "module-header" . id_) (children content)
        findM (is "caption" . class_) (children h)
      mdescription = findM (is "description" . id_) (children content)
  interface <- findM (is "interface" . id_) (children content)
  return ModuleDocs
    { mTitle = Text.unpack $ maybe "" innerText mtitle
    , mDescription = mdescription
    , mDeclarations = mapMaybe parseDeclaration $ children interface
    }

parseDeclaration :: XML.Element -> Maybe DeclarationDocs
parseDeclaration el = do
  decl <- findM (is "top" . class_) [el]
  ([sig], content) <- return
    $ partition (is "src" . class_) $ children decl
  return DeclarationDocs
    { dAnchors = Set.fromList $ anchors el
    , dSignature = sig
    , dContent = content
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
instance HasUrl Url        where getUrl url = url

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
  pkg <- P.magenta . P.text . fst <$> Hoogle.targetModule target
  mol <- P.black . P.text . fst <$> Hoogle.targetPackage target
  return $ pkg P.<+> mol

prettyModule :: ModuleDocs -> P.Doc
prettyModule (ModuleDocs name minfo decls) =
  P.vsep $ [title]
    ++ [ prettyHTML info | Just info <- [minfo] ]
    ++ [ prettyDecl decl | decl <- decls ]
  where
    title = P.vsep
      [ P.text name
      , P.text $ replicate (length name) '='
      , ""
      ]

prettyDecl :: DeclarationDocs -> P.Doc
prettyDecl DeclarationDocs{..} =
  P.vsep $ map prettyHTML $ dSignature:dContent

lookupDecl :: Anchor -> ModuleDocs -> Maybe DeclarationDocs
lookupDecl anchor (ModuleDocs _ _ decls) =
  find (Set.member anchor . dAnchors) decls

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
      ] ++ reverse (P.cyan . P.text . Hoogle.targetURL <$> toList tgroup)

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
    removeTags True  (_:xs)   = removeTags True xs

numbered :: [P.Doc] -> [P.Doc]
numbered = zipWith f [1..]
  where
    f n s = P.fill 2 (P.blue $ P.int n) P.<+> P.align s

bullet :: P.Doc -> P.Doc
bullet doc = P.fill 2 (P.char '-') <> P.align doc

-- ================================
-- Haddock handling
-- ================================

type Url = String

-- | Link to an item in a module page
data ModuleLink = ModuleLink Url (Maybe Anchor)
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

-- | Render Haddock's HTML
prettyHTML :: XML.Element -> P.Doc
prettyHTML = fromMaybe mempty . unXMLElement
  where
    unXMLElement e = style e . fold =<< unXMLChildren e
    unXMLChildren e =
      case mapMaybe unXMLNode (XML.elementNodes e) of
        [] -> Nothing
        xs -> Just xs
    unXMLNode = \case
      XML.NodeInstruction _ -> Nothing
      XML.NodeContent txt | Text.null txt -> Nothing
      XML.NodeContent txt -> Just
        $ docwords id
        $ unescapeHTML
        $ Text.unpack txt
      XML.NodeComment _ -> Nothing
      XML.NodeElement e -> unXMLElement e

    docwords f [] = P.fillCat (f [])
    docwords f (x:xs)
      | isSpace x = docwords (f . (P.space :)) $ dropWhile isSpace xs
    docwords f xs = docwords (f . (P.text w :)) ys
      where (w, ys) = break isSpace xs

    style e m = classStyle e m  >>= tagStyle e

    classStyle e = case class_ e of
      ""                  -> Just
      -- layout
      "doc"               -> Just . P.nest 2
      "subs methods"      -> Just . P.nest 2
      "subs instances"    -> Just . P.nest 2
      "subs constructors" -> Just . P.nest 2
      -- a declaration wrapper
      "top"               -> const
                              $ Just . mappend P.hardline . P.vsep
                              $ mapMaybe unXMLElement (children e)
      -- style
      "caption"           -> Just . P.bold
      "name"              -> Just . P.dullgreen
      "def"               -> Just . P.bold
      "fixity"            -> Just . italics . P.black
      -- invisible
      "link"              -> hide
      "selflink"          -> hide
      -- modify
      "module-header"     -> const $ unXMLElement =<< findM (is "caption" . class_) (children e)
      _                   -> Just

    tagStyle e = case tag e of
       "h1"      -> Just . linebreak . mappend (P.text "# ")
       "h2"      -> Just . linebreak . mappend (P.text "## ")
       "h3"      -> Just . linebreak . mappend (P.text "### ")
       "h4"      -> Just . linebreak . mappend (P.text "#### ")
       "h5"      -> Just . linebreak . mappend (P.text "##### ")
       "h6"      -> Just . linebreak . mappend (P.text "###### ")
       "tt"      -> Just . P.green
       "pre"     -> const
                      $ Just . P.nest 2 . P.black . linebreak . P.string . Text.unpack
                      $ innerText e
       "code"    -> Just . P.black
       "a"       -> Just . P.cyan
       "b"       -> Just . P.bold
       "p"       -> Just . linebreak
       "dt"      -> Just . P.bold . linebreak
       "dd"      -> Just . linebreak
       "summary" -> Just . linebreak
       "ol"      -> const $ Just . linebreak . P.vsep . numbered $ mapMaybe unXMLElement (children e)
       "ul"      -> const $ Just . linebreak . P.vsep . map bullet $ mapMaybe unXMLElement (children e)
       "td"      | isInstanceDetails e -> hide
                 | otherwise -> Just
       "table"   -> const
                      $ Just .  flip mappend P.hardline . P.vsep . map bullet
                      $ mapMaybe unXMLElement (children e)
       -- don't show instance details
       _         -> Just

    isInstanceDetails e = tag e == "td" && attr "colspan" e == "2"
    linebreak doc = P.hardline <> doc <> P.hardline
    italics = P.Italicize True
    hide = const Nothing


-- | Convert an html page into a src file and inform of line
-- number of SourceLink
fileInfo :: SourceLink -> HTML -> FileInfo
fileInfo (SourceLink _ anchor) (HTML doc) = head $ do
  let page = HTML.parseLBS doc
      root = XML.documentRoot page
  head_ <- filter (is "head" . tag) $ children root
  title <- filter (is "title" . tag) $ children head_
  let filename = Text.unpack $ Text.replace "/" "." $ innerText title <> ".hs"
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
sourceLink :: Hoogle.Target -> M (Maybe SourceLink)
sourceLink target = do
  let mlink@(ModuleLink _ manchor) = moduleLink target
  case manchor of
    Nothing -> return Nothing
    Just anchor -> do
      html <- fetch' mlink
      let links = sourceLinks mlink html
      case lookup anchor links of
        Nothing -> error $ unlines $
          [ "anchor missing in module docs"
          , show mlink
          ] ++ map show links
        Just link -> return $ Just link

-- | Get URL for module documentation
moduleLink :: Hoogle.Target -> ModuleLink
moduleLink target = ModuleLink (dropAnchor url) (takeAnchor url)
  where
    url = Hoogle.targetURL target

takeAnchor :: MonadFail m => Url -> m Anchor
takeAnchor url = case drop 1 $ dropWhile (/= '#') url of
  [] -> fail "no anchor"
  xs -> return $ Text.pack xs

dropAnchor :: Url -> Url
dropAnchor = takeWhile (/= '#')

sourceLinks :: ModuleLink -> HTML -> [(Anchor, SourceLink)]
sourceLinks (ModuleLink modLink _) (HTML html) = do
  let root = XML.documentRoot (HTML.parseLBS html)
  body        <- filter (is "body" . tag) $ children root
  content     <- filter (is "content" . id_) $ children body
  interface   <- filter (is "interface" . id_) $ children content
  declaration <- filter (is "top" . class_) $ children interface

  signature  <- findM (is "src" . class_) $ children declaration
  url <- map (toSourceUrl . attr "href")
    . findM (is "Source" . innerText)
    $ children signature
  srcAnchor <- takeAnchor url
  let surl = SourceLink (dropAnchor url) srcAnchor

  let constructors = filter (is "subs constructors" . class_) $ children declaration
  anchor <- foldMap anchors (signature : constructors)
  return (anchor, surl)
  where
    parent = reverse . tail . dropWhile (/= '/') . reverse

    toSourceUrl relativeUrl = parent modLink <> "/" <> Text.unpack relativeUrl

anchors :: XML.Element -> [Anchor]
anchors el = f $ foldMap anchors (children el)
  where
    f = if isAnchor el then (id_ el :) else id

    isAnchor e =
      class_ e == "def" &&
      (Text.isPrefixOf "t:" (id_ e) || Text.isPrefixOf "v:" (id_ e))

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


