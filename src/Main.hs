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
  , runCLI
  )

import Control.Concurrent.Async (withAsync)
import Control.Applicative (many, (<|>), optional)
import Control.Monad (void)
import qualified Network.HTTP.Client.TLS as Http (tlsManagerSettings)
import qualified Network.HTTP.Client as Http
import qualified Options.Applicative as O
import System.Environment (getEnv)
import System.FilePath.Posix ((</>))
import System.Directory (createDirectoryIfMissing)
import System.IO (hIsTerminalDevice, stdout)

import Data.Cache as Cache

data Options = Options
  { optQuery :: String
  , optAppDataDir :: Maybe FilePath
  , optUnlimitedCache :: Bool
  }

newtype AppData = AppData FilePath

-- | Create direcotry of app data if it doesn't exist
mkAppDataDir :: Maybe FilePath -> IO AppData
mkAppDataDir mpath = do
  dir <- case mpath of
    Just path -> return path
    Nothing -> do
      home <- getEnv "HOME" <|> error "HOME environment variable not set"
      return $ home </> ".haskell-docs-cli"
  createDirectoryIfMissing True dir
  return $ AppData dir

cachePolicy :: Bool -> AppData -> IO Cache.EvictionPolicy
cachePolicy unlimitedCache (AppData root) = do
  let dir = root </> "cache"
  createDirectoryIfMissing True dir
  let mb = 1024 * 1024
      (bytes, age) = if unlimitedCache
        then (Cache.NoMaxBytes         , Cache.NoMaxAge)
        else (Cache.MaxBytes $ 100 * mb, Cache.MaxAgeDays 20)
  return $ Cache.Evict bytes age (Store dir)

cliOptions :: O.ParserInfo Options
cliOptions = O.info parser $ O.header " \
  \Hoogle CLI is a command line interface to Hoogle.\
  \Hoogle is a Haskell API search engine, which allows you to search the Haskell libraries on Stackage by either function name, or by approximate type signature."
  where
    parser = do
      optQuery <- fmap unwords . many $ O.strArgument $ O.metavar "CMD"
      optAppDataDir <- optional $ O.strOption $ O.long "data-dir"
      optUnlimitedCache <- O.flag False True $ O.long "unlimited-cache"
      pure $ Options {..}

main :: IO ()
main = void $ do
  Options{..} <- O.execParser cliOptions
  manager <- Http.newManager Http.tlsManagerSettings
  appData <- mkAppDataDir optAppDataDir
  policy <- cachePolicy optUnlimitedCache appData
  cache <- Cache.create policy
  isTTY <- hIsTerminalDevice stdout
  let state = ShellState
        { sContext = ContextEmpty
        , sManager = manager
        , sCache = cache
        , sNoColours = not isTTY
        }
  withAsync (Cache.enforce policy) $ \_ ->
    runCLI state $
      case optQuery of
        ""    -> interactive
        input -> evaluate input

main' :: IO ()
main' = void $ do
  Options{} <- O.execParser cliOptions
  manager <- Http.newManager Http.tlsManagerSettings
  appData <- mkAppDataDir Nothing
  policy <- cachePolicy False appData
  cache <- Cache.create policy
  let state = ShellState
        { sContext = ContextEmpty
        , sManager = manager
        , sCache = cache
        , sNoColours = False
        }
  runCLI state $ do
    evaluateCmd (ViewDeclaration  $ Search "completeWord +haskeline")
