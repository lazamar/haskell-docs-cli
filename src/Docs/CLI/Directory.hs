module Docs.CLI.Directory where

import System.Directory (createDirectoryIfMissing, getXdgDirectory, XdgDirectory(..))
import System.FilePath


newtype AppCache = AppCache FilePath


-- | Create top directory of app cache if it doesn't exist
mkAppCacheDir :: Maybe FilePath -- ^ overwrite XDG_CACHE_HOME/haskell-docs-cli
              -> IO AppCache
mkAppCacheDir mpath = do
  dir <- (</> "html_cache") <$> case mpath of
    Just path -> return path
    Nothing -> getXdgDirectory XdgCache "haskell-docs-cli"
  createDirectoryIfMissing True dir
  return $ AppCache dir


getAppHistoryFile :: IO FilePath
getAppHistoryFile = do
  -- here's some discussion why history files belong in XDG_DATA_HOME:
  --   https://github.com/fish-shell/fish-shell/issues/744
  dir <- getXdgDirectory XdgData "haskell-docs-cli"
  createDirectoryIfMissing True dir
  return (dir </> "haskell-docs-cli.history")
