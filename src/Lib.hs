{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
  ( someFunc
  ) where

import Data.Functor ((<&>))

import qualified Hoogle
import Data.ByteString (ByteString)
import Data.Either (either)
import Control.Monad.IO.Class (liftIO)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Data.List (intersperse)

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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
      -- <$> O.strArgument
      --     (O.metavar "SEARCH"
      --     <> O.help "String to search for")
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

removeHtml :: String -> String
removeHtml = symbols . tags False
  where
    -- remove html tags
    tags _ [] = []
    tags False ('<':xs) = tags True xs
    tags True  ('>':xs) = tags False xs
    tags False (x:xs)   = x:tags False xs
    tags True  (x:xs)   = tags True xs

    -- replace html symbols
    symbols [] = []
    symbols ('&':'g':'t':';':xs) = '>':symbols xs
    symbols ('&':'l':'t':';':xs) = '<':symbols xs
    symbols ('&':'a':'m':'p':';':xs) = '<':symbols xs
    symbols (x:xs) = x:symbols xs

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
viewCompact target = concatMap removeHtml $ catMaybes
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
  $ map removeHtml
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
          Just term
            | Just n <- readMaybe term
            , x:_ <- drop (n - 1) (lastResults state) -> do
              liftIO $ putStrLn $ viewFull x
              loop state
            | otherwise -> do
              res <- liftIO $ runSearch manager options term
              let s = state { lastResults = res, lastShown = pageSize options }
              liftIO
                $ putStrLn
                $ unlines
                $ numbered
                $ take (pageSize options)
                $ map viewCompact res
              loop s

      initialState = ShellState
        { lastResults = []
        , lastShown = 0
        }

  CLI.runInputT CLI.defaultSettings $ loop initialState

numbered :: [String] -> [String]
numbered = zipWith f [1..]
  where
    f n s = show n <> ". " <> s
