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

