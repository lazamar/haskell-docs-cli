{-# LANGUAGE NumericUnderscores #-}
-- | Store results of named computations.
module Data.Cache where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (UTCTime(..), getCurrentTime, diffTimeToPicoseconds)
import Data.Time.Calendar (showGregorian)
import Data.Traversable (for)
import Data.List (isPrefixOf, find, intercalate)
import Control.Concurrent.MVar (MVar)
import Data.Map.Strict (Map)
import System.FilePath.Posix ((</>))
import System.Directory (listDirectory)

import qualified Data.ByteString.Lazy as ByteString
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map.Strict as Map
import qualified Data.Hashable as Hashable

data Cache = Cache
  { cache_eviction :: EvictionPolicy
  , cache_inFlight :: MVar (Map Hash (UTCTime, MVar ByteString))
  }

newtype Bytes = Bytes Int
newtype Days = Days Int
newtype Hash = Hash String
  deriving newtype (Show, Eq, Ord)

megabytes :: Int -> Bytes
megabytes = Bytes . (* 1000_000)

data EvictionPolicy
  = MaxSize Bytes FilePath
  | MaxAge Days FilePath
  | NoStorage

data Entry = Entry
  { entry_hash :: Hash
  , entry_time :: UTCTime
  }

newtype SerialisedEntry = SerialisedEntry String

create :: MonadIO m => EvictionPolicy -> m Cache
create policy = Cache policy <$> liftIO (MVar.newMVar mempty)

-- | Try to get result from cache. If not present, run computation.
cached :: MonadIO m => Cache -> String -> m ByteString -> m ByteString
cached cache name act = do
  now <- liftIO getCurrentTime
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
    file store = store </> fileName (serialise entry)

toEntry :: String -> UTCTime -> Entry
toEntry name time = Entry
  { entry_hash = hash
  , entry_time = time
  }
  where
    hash = Hash $ show $ abs $ Hashable.hash name

serialise :: Entry -> SerialisedEntry
serialise (Entry (Hash hash) (UTCTime day offset)) = SerialisedEntry
  $ intercalate "-"
  [ hash, showGregorian day, show (diffTimeToPicoseconds offset) ]

-- | Whether an entry an a serialised entry point to the same content.
matches :: Entry -> SerialisedEntry -> Bool
matches (Entry (Hash hash) _) (SerialisedEntry serialised) =
  hash `isPrefixOf` serialised

fileName :: SerialisedEntry -> FilePath
fileName (SerialisedEntry s) = s

retrieve :: MonadIO m => Cache -> Entry -> m (Maybe ByteString)
retrieve Cache{..} entry@(Entry hash _) = liftIO $ do
  inFlight <- fromInFlight
  case inFlight of
    Just res -> return $ Just res
    Nothing -> fromStorage
  where
    fromInFlight = do
      inFlight <- MVar.readMVar cache_inFlight
      case Map.lookup hash inFlight of
        Just (_, mvar) -> Just <$> MVar.readMVar mvar
        Nothing        -> return Nothing

    fromStorage = case cache_eviction of
      MaxSize _ path -> readFrom path
      MaxAge _ path  -> readFrom path
      NoStorage      -> return Nothing

    readFrom store = do
      stored <- fmap SerialisedEntry <$> listDirectory store
      for (find (matches entry) stored) $ \found ->
        ByteString.readFile (store </> fileName found)

