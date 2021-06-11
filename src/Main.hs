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

import Control.Applicative (many)
import Control.Monad (void)
import qualified Network.HTTP.Client.TLS as Http (tlsManagerSettings)
import qualified Network.HTTP.Client as Http
import qualified Options.Applicative as O

newtype Options = Options
  { arguments :: String
  }

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
  let state = ShellState
        { sContext = ContextEmpty
        , sManager = manager
        , sCache = mempty
        }
  runCLI state $
    case arguments of
      ""    -> interactive
      input -> evaluate input

main' :: IO ()
main' = void $ do
  Options{} <- O.execParser cliOptions
  manager <- Http.newManager Http.tlsManagerSettings
  let state = ShellState
        { sContext = ContextEmpty
        , sManager = manager
        , sCache = mempty
        }
  runCLI state $ do
    evaluateCmd (ViewModule Documentation $ Search "Data.Set")
