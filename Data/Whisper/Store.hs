{-# LANGUAGE RecordWildCards #-}
-- | Convenience API to address multiples Whisper files as a single entity.
-- But most of the time though, you will want to use Data.Whisper.Store.
module Data.Whisper.Store where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (foldl')
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (splitFileName, (<.>), (</>))

import Data.Whisper

data Store = Store
  { storeRoot :: FilePath
  -- ^ Root directory where the Whisper files are located.
  , storeLimit :: Int
  -- ^ Default number of points in the primary archive.
  , storeInterval :: Int
  -- ^ Default primary interval when creating a new Whisper file.
  }

type Timestamp = Int

newStore :: FilePath -> IO Store
newStore root = return $ Store root 1440 60 -- TODO Configuration and early check the directory exists.

-- | Push a new point to the store. Any existing value at that timestamp slot
-- is overwritten.
push :: Store -> String -> Timestamp -> Double -> IO ()
push store@Store{..} metric timestamp value = do
  let path = whisperPath store metric
  exist <- doesFileExist path
  when (not exist) $ do
    createDirectoryIfMissing True (fst $ splitFileName path)
    createWhisper path [(storeInterval, storeLimit)] 0.5 Average -- TODO configuration
  updateWhisperFile path timestamp value

readPoints store metric = do
  now <- (floor . toRational) <$> liftIO getPOSIXTime
  let path = whisperPath store metric
  wsp <- openWhisper path
  Archive points <- readArchive wsp 0 now
  closeWhisper wsp

whisperPath Store{..} metric =
  foldl' (</>) storeRoot (split '.' metric) <.> "wsp"

split :: Eq a => a -> [a] -> [[a]]
split c s =
  case break (== c) s of
    (before, []) -> [before]
    (before, _ : after) -> before : split c after

