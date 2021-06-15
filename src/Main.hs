{-# LANGUAGE ApplicativeDo #-}
module Main where

import Docs.CLI.Evaluate
  ( interactive
  , evaluate
  , evaluateCmd
  , ShellState(..)
  , Context(..)
  , Cmd(..)
  , Selection(..)
  , View(..)
  , runCLI
  )

import Control.Applicative (many, (<|>))
import Control.Monad (void)
import qualified Network.HTTP.Client.TLS as Http (tlsManagerSettings)
import qualified Network.HTTP.Client as Http
import qualified Options.Applicative as O
import System.Environment (getEnv)
import System.FilePath.Posix ((</>))
import System.Directory (createDirectoryIfMissing)

import Data.Cache as Cache

newtype Options = Options
  { arguments :: String
  }

newtype AppData = AppData FilePath

-- | Create direcotry of app data if it doesn't exist
mkAppDataDir :: IO AppData
mkAppDataDir  = do
  home <- getEnv "HOME" <|> error "HOME environment variable not set"
  let dir = home </> ".haskell-docs-cli"
  createDirectoryIfMissing True dir
  return $ AppData dir

cachePolicy :: AppData -> IO Cache.EvictionPolicy
cachePolicy (AppData root) = do
  let dir = root </> "cache"
  createDirectoryIfMissing True dir
  let mb = 1024 * 1024
      bytes = Cache.MaxBytes $ 100 * mb
      age = Cache.MaxAgeDays 20
  return $ Cache.Evict bytes age (Store dir)

cliOptions :: O.ParserInfo Options
cliOptions = O.info parser $ O.header " \
  \Hoogle CLI is a command line interface to Hoogle.\
  \Hoogle is a Haskell API search engine, which allows you to search the Haskell libraries on Stackage by either function name, or by approximate type signature."
  where
    parser = do
      arguments <- fmap unwords . many $ O.strArgument $ O.metavar "CMD"
      pure $ Options {..}

main :: IO ()
main = void $ do
  Options{..} <- O.execParser cliOptions
  manager <- Http.newManager Http.tlsManagerSettings
  appData <- mkAppDataDir
  policy <- cachePolicy appData
  cache <- Cache.create policy
  let state = ShellState
        { sContext = ContextEmpty
        , sManager = manager
        , sCache = cache
        }
  runCLI state $
    case arguments of
      ""    -> interactive
      input -> evaluate input

main' :: IO ()
main' = void $ do
  Options{} <- O.execParser cliOptions
  manager <- Http.newManager Http.tlsManagerSettings
  appData <- mkAppDataDir
  policy <- cachePolicy appData
  cache <- Cache.create policy
  let state = ShellState
        { sContext = ContextEmpty
        , sManager = manager
        , sCache = cache
        }
  runCLI state $ do
    evaluateCmd (ViewModule Documentation $ Search "Data.Set")
