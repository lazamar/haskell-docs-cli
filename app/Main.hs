module Main where

import HoogleCli (interactive, evaluate, ShellState(..), Context(..), Cmd(..), Selection(..), runCLI)

import Control.Monad (void)
import qualified Network.HTTP.Client.TLS as Http (tlsManagerSettings)
import qualified Network.HTTP.Client as Http
import qualified Options.Applicative as O
import qualified System.Console.Haskeline as CLI

newtype Options = Options ()

cliOptions :: O.ParserInfo Options
cliOptions = O.info parser $ O.header " \
  \Hoogle CLI is a command line interface to Hoogle.\
  \Hoogle is a Haskell API search engine, which allows you to search the Haskell libraries on Stackage by either function name, or by approximate type signature."
  where
    parser = pure $ Options ()

main :: IO ()
main = void $ do
  Options () <- O.execParser cliOptions
  manager <- Http.newManager Http.tlsManagerSettings
  let state = ShellState
        { sContext = ContextEmpty
        , sManager = manager
        , sCache = mempty
        }
  runCLI CLI.defaultSettings state interactive

main' :: IO ()
main' = void $ do
  Options () <- O.execParser cliOptions
  manager <- Http.newManager Http.tlsManagerSettings
  let state = ShellState
        { sContext = ContextEmpty
        , sManager = manager
        , sCache = mempty
        }
  runCLI CLI.defaultSettings state $ do
    evaluate (DefaultCmd $ Search "Data.Set")
    evaluate (DefaultCmd $ ItemIndex 1)
