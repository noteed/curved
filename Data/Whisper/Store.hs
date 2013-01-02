{-# LANGUAGE RecordWildCards #-}
-- | Convenience API to address multiples Whisper files as a single entity.
-- But most of the time though, you will want to use Data.Whisper.Store.
module Data.Whisper.Store
  ( newStore
  , push
  , readPoints
  )where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (foldl')
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (splitFileName, (<.>), (</>))

import Data.Whisper

-- | The `Store` acts as a handle. One a `Store` has been configured and
-- created with `newStore`, it is possible to use it to `push` new metrics
-- data and retrieve them, e.g. with `readPoints`.
data Store = Store
  { storeRoot :: FilePath
  -- ^ Root directory where the Whisper files are located.
  , storeLimit :: Int
  -- ^ Default number of points in the primary archive.
  , storeInterval :: Int
  -- ^ Default primary interval when creating a new Whisper file.
  , storePropagationFactor :: Float
  -- ^ Default propagation factor when creating a new Whisper file.
  , storeAggregationMethod :: AggregationType
  -- ^ Default aggregation method when creating a new Whisper file.
  }

type Timestamp = Int

-- | Create a new `Store` handle to access the underlying storage.
-- TODO Configuration and early check the directory exists.
newStore :: FilePath -> IO Store
newStore root = return $ Store root 1440 60 0.5 Average

-- | Push a new point to the store. Any existing value at that timestamp slot
-- is overwritten.
push :: Store -> String -> Timestamp -> Double -> IO ()
push store@Store{..} metric timestamp value = do
  let path = whisperPath store metric
  exist <- doesFileExist path
  when (not exist) $ do
    createDirectoryIfMissing True (fst $ splitFileName path)
    createWhisper path storePropagationFactor storeAggregationMethod
      [(storeInterval, storeLimit)]
  updateWhisperFile path timestamp value

-- | Read the points associated to a metric name.
readPoints :: Store -> String -> IO [Point]
readPoints store metric = do
  now <- (floor . toRational) <$> liftIO getPOSIXTime
  let path = whisperPath store metric
  wsp <- openWhisper path
  Archive points <- readArchive wsp 0 now
  closeWhisper wsp
  return points

-- | Convert a metric name to its location on disk.
whisperPath :: Store -> String -> FilePath
whisperPath Store{..} metric =
  foldl' (</>) storeRoot (split '.' metric) <.> "wsp"

-- | Split a list on a given separator.
split :: Eq a => a -> [a] -> [[a]]
split c s =
  case break (== c) s of
    (before, []) -> [before]
    (before, _ : after) -> before : split c after

