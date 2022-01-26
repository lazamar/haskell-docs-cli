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
  , HackageUrl(..)
  , HoogleUrl(..)
  , runCLI
  , defaultHackageUrl
  , defaultHoogleUrl
  , moreInfoText
  )

import Control.Concurrent.Async (withAsync)
import Control.Applicative (many, (<|>), optional)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Network.HTTP.Client.TLS as Http (tlsManagerSettings)
import qualified Network.HTTP.Client as Http
import qualified Options.Applicative as O
import qualified Options.Applicative.Help.Pretty as OP
import System.Environment (getEnv)
import System.FilePath.Posix ((</>))
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.IO (hIsTerminalDevice, stdout)

import Data.Cache as Cache

data CacheOption = Unlimited | Off

data Options = Options
  { optQuery :: String
  , optAppDataDir :: Maybe FilePath
  , optCache :: Maybe CacheOption
  , optHoogle :: Maybe HoogleUrl
  , optHackage :: Maybe HackageUrl
  }

newtype AppData = AppData FilePath

-- | Create direcotry of app data if it doesn't exist
mkAppDataDir :: Maybe FilePath -> IO AppData
mkAppDataDir mpath = do
  dir <- case mpath of
    Just path -> return path
    Nothing -> do
      home <- getHomeDirectory
      return $ home </> ".haskell-docs-cli"
  createDirectoryIfMissing True dir
  return $ AppData dir

cachePolicy :: Maybe CacheOption -> AppData -> IO Cache.EvictionPolicy
cachePolicy mCacheOpt (AppData root) =
  case mCacheOpt of
    Just Off -> return Cache.NoStorage
    Just Unlimited -> eviction Cache.NoMaxBytes Cache.NoMaxAge
    Nothing -> eviction (Cache.MaxBytes $ 100 * mb) (Cache.MaxAgeDays 20)
  where
    mb = 1024 * 1024
    eviction bytes age = do
      let dir = root </> "cache"
      createDirectoryIfMissing True dir
      return $ Cache.Evict bytes age (Store dir)

cliOptions :: O.ParserInfo Options
cliOptions = O.info (O.helper <*> parser) $ mconcat
  [ O.fullDesc
  , O.headerDoc $ Just $ OP.vcat
    [ "haskell-docs-cli"
    , ""
    , OP.indent 2 $ OP.vcat
      [ "Search Hoogle and view Hackage documentation from the command line."
      , "Search modules, packages, types and functions by name or by approximate type signature."
      ]
    ]
  , O.footerDoc $ Just $ moreInfoText <> OP.linebreak
  ]
  where
    parser = do
      optQuery <- fmap unwords . many $ O.strArgument $ O.metavar "CMD"
      optAppDataDir <- optional $ O.strOption $ mconcat
        [ O.long "data-dir"
        , O.metavar "PATH"
        , O.help "Specify the directory for application data such as requests cache to be stored."
        ]
      optCache <- optional $ O.option readCache $ mconcat
        [ O.long "cache"
        , O.metavar "unlimited|off"
        , O.help "Set a custom cache eviction policy"
        ]
      optHoogle <- optional $ fmap HoogleUrl $ O.strOption $ mconcat
        [ O.long "hoogle"
        , O.metavar "URL"
        , O.help "Address of Hoogle instance to be used"
        ]
      optHackage <- optional $ fmap HackageUrl $ O.strOption $ mconcat
        [ O.long "hackage"
        , O.metavar "URL"
        , O.help "Address of Hackage instance to be used"
        ]
      pure $ Options {..}
      where
        readCache  = O.maybeReader $ \str ->
          case str of
            "unlimited" -> Just Unlimited
            "off" -> Just Off
            _ -> Nothing


main :: IO ()
main = void $ do
  Options{..} <- O.execParser cliOptions
  manager <- Http.newManager Http.tlsManagerSettings
  appData <- mkAppDataDir optAppDataDir
  policy <- cachePolicy optCache appData
  cache <- Cache.create policy
  isTTY <- hIsTerminalDevice stdout
  let state = ShellState
        { sContext = ContextEmpty
        , sManager = manager
        , sCache = cache
        , sNoColours = not isTTY
        , sHoogle = fromMaybe defaultHoogleUrl optHoogle
        , sHackage = fromMaybe defaultHackageUrl optHackage
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
  policy <- cachePolicy Nothing appData
  cache <- Cache.create policy
  let state = ShellState
        { sContext = ContextEmpty
        , sManager = manager
        , sCache = cache
        , sNoColours = False
        , sHoogle = defaultHoogleUrl
        , sHackage = defaultHackageUrl
        }
  runCLI state $ do
    evaluateCmd (ViewDeclaration  $ Search "completeWord +haskeline")
