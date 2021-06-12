{-# LANGUAGE NumericUnderscores #-}
-- | Store results of named computations.
module Data.Cache where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (secondsToNominalDiffTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Hashable (hash)
import Data.Maybe (mapMaybe)
import Data.Bifunctor (second)
import Control.Concurrent.MVar (MVar)
import Data.Map.Strict (Map)
import System.FilePath.Posix ((</>))
import System.Directory (listDirectory)
import Text.Read (readMaybe)

import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString.Lazy as ByteString
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map.Strict as Map

data Cache = Cache
  { cache_eviction :: EvictionPolicy
  , cache_inFlight :: MVar (Map Hash (POSIXTime, MVar ByteString))
  }

newtype Bytes = Bytes Int
newtype Days = Days Int
newtype Hash = Hash Int
  deriving newtype (Show, Eq, Ord)

megabytes :: Int -> Bytes
megabytes = Bytes . (* 1000_000)

data EvictionPolicy
  = MaxSize Bytes FilePath
  | MaxAge Days FilePath
  | NoStorage

data Entry = Entry
  { entry_hash :: Hash
  , entry_time :: POSIXTime
  }

create :: MonadIO m => EvictionPolicy -> m Cache
create policy = Cache policy <$> liftIO (MVar.newMVar mempty)

-- | Try to get result from cache. If not present, run computation.
cached :: MonadIO m => Cache -> String -> m ByteString -> m ByteString
cached cache name act = do
  now <- liftIO getPOSIXTime
  let entry = toEntry name now
  mcontent <- retrieve cache entry
  case mcontent of
    Just content -> return content
    Nothing -> do
      var <- liftIO MVar.newEmptyMVar
      liftIO
        $ MVar.modifyMVar_ (cache_inFlight cache)
        $ return . Map.insert (entry_hash entry) (now, var)
      content <- act
      liftIO $ MVar.putMVar var content
      save cache entry content
      return content

save :: MonadIO m => Cache -> Entry -> ByteString -> m ()
save Cache{..} entry content =
  liftIO $ case cache_eviction of
    MaxSize _ path -> ByteString.writeFile (file path) content
    MaxAge _ path -> ByteString.writeFile (file path) content
    NoStorage      -> return ()
  where
    file store = store </> encode entry

toEntry :: String -> POSIXTime -> Entry
toEntry = Entry . Hash . abs . hash

encode :: Entry -> String
encode (Entry (Hash h) time) = show h <> "-" <> show (nominalDiffTimeToSeconds time)

decode :: String -> Maybe Entry
decode str = do
  h <- readMaybe hash'
  t <- readMaybe time
  return $ Entry (Hash h) (secondsToNominalDiffTime t)
  where
    (hash', time) = second (drop 1) $ break (== '-') str

retrieve :: MonadIO m => Cache -> Entry -> m (Maybe ByteString)
retrieve Cache{..} (Entry h _) = liftIO $ do
  inFlight <- fromInFlight
  case inFlight of
    Just res -> return $ Just res
    Nothing -> fromStorage
  where
    fromInFlight = do
      inFlight <- MVar.readMVar cache_inFlight
      case Map.lookup h inFlight of
        Just (_, mvar) -> Just <$> MVar.readMVar mvar
        Nothing           -> return Nothing

    fromStorage = case cache_eviction of
      MaxSize _ path -> readFrom path
      MaxAge _ path -> readFrom path
      NoStorage      -> return Nothing

    readFrom store = do
      fnames <- listDirectory store
      let entries = mapMaybe decode fnames
      case [ e | e@(Entry h' _) <- entries , h' == h ] of
        (entry:_) -> Just <$> ByteString.readFile (store </> encode entry)
        _     -> return Nothing


