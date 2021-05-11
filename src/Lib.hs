{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Lib where

import Debug.Trace
import Data.Functor ((<&>))
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Either (either)
import Data.List.Extra (unescapeHTML)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes, fromMaybe, fromJust)
import Data.List (intersperse, intercalate, foldl')
import Data.Char (chr, ord)
import Data.Set (Set)
import Data.Text (Text)
import System.IO (hPutStr)
import System.IO.Temp
  ( withSystemTempDirectory
  , withSystemTempFile
  , withTempFile
  , withTempDirectory
  , getCanonicalTemporaryDirectory)
import qualified System.Process as Process
import qualified Hoogle
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import qualified Text.XML as XML
import qualified Text.HTML.DOM as HTML
import qualified Data.Set as Set
import qualified Data.Aeson as Aeson
import qualified Data.Text.IO as Text (hPutStr)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.Encoding as LText
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http (tlsManagerSettings)
import qualified Options.Applicative as O
import qualified System.Console.Haskeline as CLI

data Options = Options
  { count :: Int
  , pageSize :: Int
  }

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

unHTML :: String -> String
unHTML = unescapeHTML . removeTags False
  where
    -- remove html tags
    removeTags _ [] = []
    removeTags False ('<':xs) = removeTags True xs
    removeTags True  ('>':xs) = removeTags False xs
    removeTags False (x:xs)   = x:removeTags False xs
    removeTags True  (x:xs)   = removeTags True xs


runSearch :: Http.Manager -> Options -> String -> IO [Hoogle.Target]
runSearch manager Options{..} term = do
  req <- Http.parseRequest "https://hoogle.haskell.org"
    <&> Http.setQueryString
      [ ("mode", Just "json")
      , ("start", Just "1")
      , ("hoogle", Just $ bs term)
      ]
  res <- Http.httpLbs req manager
  either error return $ Aeson.eitherDecode (Http.responseBody res)
  where
    bs :: String -> ByteString
    bs = Text.encodeUtf8 . Text.pack

viewCompact :: Hoogle.Target -> String
viewCompact target = concatMap unHTML $ catMaybes
  [ Just $ Hoogle.targetItem target
  , moduleName
  ]
  where
    moduleName = do
      pkg <- fst <$> Hoogle.targetPackage target
      mod <- fst <$> Hoogle.targetModule target
      return $ "\n" ++ pkg ++ " " ++ mod

viewFull :: Hoogle.Target -> String
viewFull target
  = unlines
  $ map unHTML
  $ intersperse ""
  $ filter (not . null)
  [ Hoogle.targetItem target
  , maybe "" fst $ Hoogle.targetPackage target
  , maybe "" fst $ Hoogle.targetModule target
  , Hoogle.targetDocs target
  , Hoogle.targetURL target
  ]

data ShellState = ShellState
  { lastResults :: [Hoogle.Target]
  , lastShown :: Int
  }


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x xs = y : splitOn x (drop 1 ys)
  where
    (y, ys) = break (== x) xs

someFunc :: IO ()
someFunc = do
  options <- O.execParser cliOptions
  manager <- Http.newManager Http.tlsManagerSettings
  let loop :: ShellState -> CLI.InputT IO ()
      loop state = do
        minput <- CLI.getInputLine "hoogle> "
        case minput of
          Nothing -> return ()
          Just "q" -> return ()
          Just "quit" -> return ()
          Just (x:"source")
            | Just n <- readMaybe [x] -> do
            liftIO $ do
              let target = lastResults state !! (n - 1)
              (anchor, url) <- sourceUrl manager target
              res <- get manager url
              let (filename, mline, content) = fileInfo anchor res
                  line = maybe "" (("+" <>) . show) mline
              withSystemTempFile filename $ \fullpath handle -> do
                Text.hPutStr handle content
                Process.callCommand $ traceShowId $ unwords ["nvim ", fullpath, line]
            loop state

          Just term
            | Just n <- readMaybe term -> do
              details state (n - 1)
              loop state
            | otherwise -> do
              res <- liftIO $ runSearch manager options term
              liftIO
                $ putStrLn
                $ unlines
                $ reverse
                $ numbered
                $ take (pageSize options)
                $ map viewCompact res
              loop state { lastResults = res, lastShown = pageSize options }

      details :: ShellState -> Int -> CLI.InputT IO ()
      details state ix = liftIO $ do
        divider
        putStrLn $ viewFull $ lastResults state !! ix
        divider

      initialState = ShellState
        { lastResults = []
        , lastShown = 0
        }

  CLI.runInputT CLI.defaultSettings $ loop initialState

withTempPath :: String -> (String -> IO a) -> IO a
withTempPath path f = do
  root <- getCanonicalTemporaryDirectory
  go root $ filter (not . null) $ splitOn '/' path
  where
    go parent [] = f parent
    go parent (x:xs) =
      withTempDirectory parent x $ \p -> go p xs



divider :: IO ()
divider = putStrLn $ replicate 50 '='

numbered :: [String] -> [String]
numbered = zipWith f [1..]
  where
    f n s = show n <> ". " <> s

get :: Http.Manager -> String -> IO LB.ByteString
get manager url = do
  req <- Http.parseRequest url
  res <- Http.httpLbs req manager
  return $ Http.responseBody res

-- Haddock handling

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
sourceUrl :: Http.Manager -> Hoogle.Target -> IO (Anchor, Url)
sourceUrl manager target = do
  docs <- get manager $ dropAnchor docsUrl
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

