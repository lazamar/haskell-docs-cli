{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib where

import Data.Functor ((<&>))

import Data.ByteString (ByteString)
import Data.Either (either)
import Data.List.Extra (unescapeHTML)
import Control.Monad.IO.Class (liftIO)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes, fromMaybe, fromJust)
import Data.List (intersperse, intercalate)
import Data.Char (chr, ord)
import Data.Set (Set)
import Data.Text (Text)
import System.IO (hPutStr)
import System.IO.Temp (withSystemTempFile)

import qualified System.Process as Process
import qualified Hoogle
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import qualified Text.XML as XML
import qualified Text.HTML.DOM as HTML
import qualified Data.Set as Set
import qualified Data.Aeson as Aeson
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
              (filename, url) <- sourceUrl manager target
              res <- get manager url
              let content = unHTML $ LText.unpack $ LText.decodeUtf8 res
              withSystemTempFile filename $ \path handle -> do
                hPutStr handle content
                Process.callCommand $ "vim " <> path
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

type Anchor = Text

newtype RelativeUrl = RelativeUrl Text

-- | Get URL for source file for a target
sourceUrl :: Http.Manager -> Hoogle.Target -> IO (FileName, Url)
sourceUrl manager target = do
  docs <- get manager $ dropAnchor docsUrl
  let links = sourceLinks (HTML.parseLBS docs) :: [(Anchor, RelativeUrl)]
      url = toAbsoluteUrl $ fromJust $ lookup anchor links
      filename = reverse $ takeWhile (/= '/') $ reverse url
  return (filename, url)
  where
    docsUrl = Hoogle.targetURL target
    anchor = Text.pack $ takeAnchor $ Hoogle.targetURL target

    dropAnchor = takeWhile (/= '#')
    takeAnchor = tail . dropWhile (/= '#')

    parent = reverse . tail . dropWhile (/= '/') . reverse

    toAbsoluteUrl (RelativeUrl url) = parent docsUrl <> "/" <> Text.unpack url

sourceLinks :: XML.Document -> [(Anchor, RelativeUrl)]
sourceLinks root = do
  body       <- filter (is "body" . tag) $ children $ XML.documentRoot root
  content    <- filter (is "content" . id_) $ children body
  interface  <- filter (is "interface" . id_) $ children content
  definition <- filter (is "top" . class_) $ children interface

  signature  <- filter (is "src" . class_) $ children definition
  sourceUrl  <- map (attr "href") . filter (is ["Source"] . innerText)
     $ children signature
  let constructors = filter (is "subs constructors" . class_) $ children definition

  anchor <- foldMap (all anchors) (signature : constructors)
  return (anchor, RelativeUrl sourceUrl)
  where
    is a b = a == b

    children element =
      [ n | XML.NodeElement n <- XML.elementNodes element ]

    tag = XML.nameLocalName . XML.elementName

    id_ = attr "id"

    class_ = attr "class"

    attr name =
      fromMaybe ""
      . Map.lookup (XML.Name name Nothing Nothing)
      . XML.elementAttributes

    innerText el = [ c | XML.NodeContent c <- XML.elementNodes el ]

    isAnchor x = Text.isPrefixOf "t:" x || Text.isPrefixOf "v:" x

    all f el = foldr (mappend . all f) (f el) (children el)

    anchors el =
      if isAnchor (id_ el) && class_ el == "def"
         then pure $ id_ el
         else mempty

