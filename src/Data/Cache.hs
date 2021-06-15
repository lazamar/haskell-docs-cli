{-# LANGUAGE NumericUnderscores #-}
-- | Store results of named computations.
module Data.Cache
  ( create
  , cached
  , enforce
  , Store(..)
  , EvictionPolicy(..)
  , Cache
  , MaxBytes(..)
  , MaxAgeDays(..)
  )
  where

import Control.Exception (try, throwIO, fromException, SomeAsyncException(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (void)
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (UTCTime(..))
import Data.Traversable (for)
import Data.Foldable (traverse_)
import Data.List (isPrefixOf, find, intercalate, sortOn)
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Concurrent.MVar (MVar)
import Data.Map.Strict (Map)
import System.FilePath.Posix ((</>))
import System.Directory (listDirectory, removeFile, getFileSize)
import Text.Read (readMaybe)

import qualified Data.Time.Clock as Time
import qualified Data.Time.Calendar as Time
import qualified Data.ByteString.Lazy as ByteString
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map.Strict as Map
import qualified Data.Hashable as Hashable

data Cache = Cache
  { cache_eviction :: EvictionPolicy
  , cache_inFlight :: MVar (Map Hash (UTCTime, MVar ByteString))
  }

newtype Store = Store FilePath
newtype Hash = Hash Int
  deriving newtype (Show, Eq, Ord)

data EvictionPolicy
  = Evict MaxBytes MaxAgeDays Store
  | NoStorage

newtype MaxBytes   = MaxBytes Integer
newtype MaxAgeDays = MaxAgeDays Int

data Entry = Entry
  { entry_hash :: Hash
  , entry_time :: UTCTime
  }
  deriving (Show, Eq)

newtype SerialisedEntry = SerialisedEntry String
  deriving Show

create :: MonadIO m => EvictionPolicy -> m Cache
create policy = Cache policy <$> liftIO (MVar.newMVar mempty)

-- | Try to get result from cache. If not present, run computation.
cached :: MonadIO m => Cache -> String -> m ByteString -> m ByteString
cached cache name act = do
  now <- liftIO Time.getCurrentTime
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

enforce :: MonadIO m => EvictionPolicy -> m ()
enforce = \case
  NoStorage -> return ()
  Evict maxSize maxAge store -> liftIO $ do
    serialised <- readEntriesFrom store
    let entries = sortOn entry_time $ mapMaybe deserialise serialised
    oversize <- overLimit store maxSize entries
    overage  <- overAge maxAge entries
    traverse_ (remove store) $ oversize ++ overage
  where
    overLimit :: Store -> MaxBytes -> [Entry] -> IO [Entry]
    overLimit _ _ [] = return []
    overLimit store (MaxBytes bytes) (entry:rest) = do
      s <- fromMaybe 0 <$> size store entry
      let remaining = bytes - s
      if remaining >= 0
        then overLimit store (MaxBytes remaining) rest
        else (entry:) <$> overLimit store (MaxBytes bytes) rest

    overAge :: MaxAgeDays -> [Entry] -> IO [Entry]
    overAge (MaxAgeDays days) entries = do
      now <- Time.getCurrentTime
      let threshold = Time.addUTCTime (-Time.nominalDay * fromIntegral days) now
      return $ filter (olderThan threshold) entries

    olderThan :: UTCTime -> Entry -> Bool
    olderThan threshold (Entry _ time) =
      time < threshold

    remove :: Store -> Entry -> IO ()
    remove store = void . trySync . removeFile . location store

    size :: Store -> Entry -> IO (Maybe Integer)
    size store = trySync. getFileSize . location store

-- | Catch synchronous exceptions and return Nothing
-- Asynchronous exceptions will kill the action
trySync :: IO a -> IO (Maybe a)
trySync act = do
  res <- try act
  case res of
    Left e | Just (SomeAsyncException _) <- fromException e -> throwIO e
           | otherwise -> return Nothing
    Right r -> return $ Just r

location :: Store -> Entry -> FilePath
location store = fileName store . serialise

fileName :: Store -> SerialisedEntry -> FilePath
fileName (Store path) (SerialisedEntry s) = path </> s

storePath :: Cache -> Maybe Store
storePath Cache{..} =
  case cache_eviction of
    Evict _ _ store -> Just store
    NoStorage       -> Nothing

save :: MonadIO m => Cache -> Entry -> ByteString -> m ()
save cache entry content
  | Just store <- storePath cache
  = liftIO $ ByteString.writeFile (location store entry) content
  | otherwise
  = return ()

toEntry :: String -> UTCTime -> Entry
toEntry name time = Entry
  { entry_hash = hash
  , entry_time = time
  }
  where
    hash = Hash $ abs $ Hashable.hash name

serialise :: Entry -> SerialisedEntry
serialise (Entry (Hash hash) (UTCTime day offset)) = SerialisedEntry
  $ intercalate [separator]
  [ show hash, Time.showGregorian day, show (Time.diffTimeToPicoseconds offset) ]

separator :: Char
separator = '-'

deserialise :: SerialisedEntry -> Maybe Entry
deserialise (SerialisedEntry str) =
  case mapMaybe readMaybe $ splitBy separator str of
  [h, year, month, day, offset] ->
    let days = Time.fromGregorian (toInteger year) month day
        diff = Time.picosecondsToDiffTime $ toInteger offset
    in
    return $ Entry (Hash h) (UTCTime days diff)
  _ -> Nothing
  where
    splitBy _ [] = []
    splitBy x xs = case drop 1 <$> break (== x) xs of
      ([]  , rest) -> splitBy x rest
      (part, rest) -> part : splitBy x rest

-- | Whether an entry an a serialised entry point to the same content.
matches :: SerialisedEntry -> SerialisedEntry -> Bool
matches (SerialisedEntry a) (SerialisedEntry b) =
  takeWhile (/= separator) a `isPrefixOf` b

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
      Evict _ _ path -> readFrom path
      NoStorage      -> return Nothing

    readFrom store = do
      stored <- readEntriesFrom store
      for (find (matches $ serialise entry) stored) $ \found ->
        ByteString.readFile (fileName store found)

readEntriesFrom :: MonadIO m => Store -> m [SerialisedEntry]
readEntriesFrom (Store path) =
  fmap (fmap SerialisedEntry) $ liftIO $ listDirectory path
