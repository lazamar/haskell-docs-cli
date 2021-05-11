{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Lib where

import Debug.Trace

import Control.Monad.Trans.Class (lift)
import Control.Concurrent.MVar (MVar)
import Control.Monad.Trans.State.Lazy (StateT)
import Data.Functor ((<&>))
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (ByteString)
import Data.Either (either)
import Data.List.Extra (unescapeHTML)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes, fromMaybe, fromJust)
import Data.List (intersperse, intercalate, foldl')
import Data.Char (chr, ord)
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


import qualified Control.Monad.Trans.State.Lazy as State
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LB
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
import qualified Text.XML as XML
import qualified Text.PrettyPrint.ANSI.Leijen as P

data Options = Options
  { count :: Int
  , pageSize :: Int
  }

data ShellState = ShellState
  { sLastResults :: [Hoogle.Target]
  , sLastShown :: Int
  , sManager :: Http.Manager
  , sCache :: Map Url (MVar ByteString)
  , sOptions :: Options
  }

type M a = StateT ShellState (CLI.InputT IO) a

defaultPageSize = 20

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

loop :: M ()
loop = do
  minput <- lift $ CLI.getInputLine "hoogle> "
  case minput of
    Nothing -> return ()
    Just "q" -> return ()
    Just "quit" -> return ()
    Just (x:"source") | Just n <- readMaybe [x] -> do
      results <- State.gets sLastResults
      let target = results !! (n - 1)
      (anchor, url) <- sourceUrl target
      res <- fetch' url
      let (filename, mline, content) = fileInfo anchor res
          line = maybe "" (("+" <>) . show) mline
      liftIO $ withSystemTempFile filename $ \fullpath handle -> do
        Text.hPutStr handle content
        Process.callCommand $ traceShowId $ unwords ["nvim ", fullpath, line]
      loop

    Just term | Just n <- readMaybe term -> do
      results <- State.gets sLastResults
      liftIO $ do
        P.putDoc $ viewFull $ results !! (n - 1)
        putStrLn ""
      loop

    Just term -> do
      options <- State.gets sOptions
      res <- runSearch term
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
      loop

withTempPath :: String -> (String -> IO a) -> IO a
withTempPath path f = do
  root <- getCanonicalTemporaryDirectory
  go root $ filter (not . null) $ splitOn '/' path
  where
    go parent [] = f parent
    go parent (x:xs) =
      withTempDirectory parent x $ \p -> go p xs

fetch' :: Url -> M ByteString
fetch' url = fetch =<< liftIO (Http.parseRequest url)

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

viewModule :: Hoogle.Target -> Maybe P.Doc
viewModule target = do
  mod <- fst <$> Hoogle.targetModule target
  return $ P.magenta (P.text mod)

viewPackage :: Hoogle.Target -> Maybe P.Doc
viewPackage target = do
  pkg <- fst <$> Hoogle.targetPackage target
  return $ P.black (P.text pkg)

viewItem :: Hoogle.Target -> P.Doc
viewItem target = prettyHTML $ Hoogle.targetItem target

viewCompact :: Hoogle.Target -> P.Doc
viewCompact target = P.vsep
  [ viewItem target
  , moduleName
  ]
  where
    moduleName = fromMaybe mempty $ do
      pkg <- viewPackage target
      mod <- viewModule target
      return $ pkg P.<+> mod

prettyHTML :: String -> P.Doc
prettyHTML item = unXMLElement doc
  where
    doc = XML.documentRoot $ HTML.parseLBS $ bs $ "<p>" <> item <> "</p>"
    bs = LB.fromStrict . Text.encodeUtf8 . Text.pack
    unXMLNode = \case
      XML.NodeInstruction _ -> mempty
      XML.NodeContent txt -> P.text $ unescapeHTML $ Text.unpack txt
      XML.NodeComment _ -> mempty
      XML.NodeElement e -> unXMLElement e
    unXMLElement e = modifier e $ foldMap unXMLNode $ XML.elementNodes e
    modifier e = tagModifier e . classModifier e
    classModifier e = case class_ e of
      "name" -> P.dullgreen
      _ -> id
    tagModifier e = case tag e of
       "a" -> P.cyan
       "tt" -> P.green
       "pre" -> P.black
       "b" -> P.bold
       _ -> id

viewFull :: Hoogle.Target -> P.Doc
viewFull target = P.vsep
  [ divider
  , content
  , divider
  ]
  where
    divider = P.black $ P.text $ replicate 50 '='
    content = P.vsep $ catMaybes
      [ Just $ viewItem target
      , viewPackage target
      , viewModule target
      , Just . prettyHTML $ Hoogle.targetDocs target
      , Just . P.cyan . P.text $ Hoogle.targetURL target
      ]

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

-- ================================
-- Haddock handling
-- ================================

type Url = String

type FileName = String

type FileContent = Text

type Anchor = Text

newtype RelativeUrl = RelativeUrl Text

fileInfo :: Anchor -> LB.ByteString -> (FileName, Maybe Int, FileContent)
fileInfo anchor doc = head $ do
  let page = HTML.parseLBS doc
      root = XML.documentRoot page
  head_ <- filter (is "head" . tag) $ children root
  title <- filter (is "title" . tag) $ children head_
  let filename = Text.unpack $ Text.replace "/" "." $ innerText title
  body <- filter (is "body" . tag) $ children root
  return (filename, anchorLine anchor body, innerText body)

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
sourceUrl :: Hoogle.Target -> M (Anchor, Url)
sourceUrl target = do
  docs <- fetch' (dropAnchor docsUrl)
  let links = sourceLinks (HTML.parseLBS docs)
      url = toAbsoluteUrl $ fromJust $ lookup anchor links
  return (takeAnchor url, url)
  where
    docsUrl = Hoogle.targetURL target
    anchor = takeAnchor $ Hoogle.targetURL target
    dropAnchor = takeWhile (/= '#')
    parent = reverse . tail . dropWhile (/= '/') . reverse
    toAbsoluteUrl (RelativeUrl url) = parent docsUrl <> "/" <> Text.unpack url

takeAnchor :: Url -> Anchor
takeAnchor = Text.pack . tail . dropWhile (/= '#')

sourceLinks :: XML.Document -> [(Anchor, RelativeUrl)]
sourceLinks doc = do
  let root = XML.documentRoot doc
  body       <- filter (is "body" . tag) $ children root
  content    <- filter (is "content" . id_) $ children body
  interface  <- filter (is "interface" . id_) $ children content
  definition <- filter (is "top" . class_) $ children interface

  signature  <- filter (is "src" . class_) $ children definition
  sourceUrl  <- map (attr "href") . filter (is "Source" . innerText)
     $ children signature
  let constructors = filter (is "subs constructors" . class_) $ children definition

  anchor <- foldMap anchors (signature : constructors)
  return (anchor, RelativeUrl sourceUrl)
  where
    anchors el = f $ foldMap anchors (children el)
      where
        f = if isAnchor el then (id_ el :) else id

    isAnchor el =
      class_ el == "def" &&
      (Text.isPrefixOf "t:" (id_ el) || Text.isPrefixOf "v:" (id_ el))

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


