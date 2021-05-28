module Main where

import HoogleCli (interactive, evaluate, ShellState(..), Context(..), Cmd(..))

import qualified Network.HTTP.Client.TLS as Http (tlsManagerSettings)
import qualified Network.HTTP.Client as Http
import qualified Options.Applicative as O
import qualified System.Console.Haskeline as CLI
import qualified Control.Monad.Trans.State.Lazy as State

newtype Options = Options ()

cliOptions :: O.ParserInfo Options
cliOptions = O.info parser $ O.header " \
  \Hoogle CLI is a command line interface to Hoogle.\
  \Hoogle is a Haskell API search engine, which allows you to search the Haskell libraries on Stackage by either function name, or by approximate type signature."
  where
    parser = pure $ Options ()

main :: IO ()
main = do
  Options () <- O.execParser cliOptions
  manager <- Http.newManager Http.tlsManagerSettings
  CLI.runInputT CLI.defaultSettings
    $ State.evalStateT interactive ShellState
        { sContext = ContextEmpty
        , sManager = manager
        , sCache = mempty
        }


main' :: IO ()
main' = do
  Options () <- O.execParser cliOptions
  manager <- Http.newManager Http.tlsManagerSettings
  CLI.runInputT CLI.defaultSettings
    $ State.evalStateT
      (do
        evaluate (Search "Data.Set")
        evaluate (ViewDocs 1)
      )
      ShellState
        { sContext = ContextEmpty
        , sManager = manager
        , sCache = mempty
        }
