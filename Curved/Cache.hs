{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
-- | The cache provides an API to monitor values in an application. The values
-- can exist only in memory, and/or be written to disk, and can be forwarded to a
-- metric gathering tool.
-- It is a layer above Data.Whisper.Store.
module Curved.Cache where

import Control.Applicative ((<$>))
import qualified Data.HashMap.Strict as M
import Data.IORef (atomicModifyIORef, newIORef, readIORef, IORef)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)

import Data.Whisper (Point(..))

-- TODO The counter defined here is very similar to the Series defined in
-- Numbers.Whisper.Series. Some code sharing is needed.

data Store = Store
  { storeCounters :: !(IORef (M.HashMap T.Text Counter))
  , storeRoot :: Maybe FilePath
  -- ^ If data must be written to disk, root directory of the data store.
  , storeLimit :: Int
  -- ^ Default cache size used when creating a new counter. Set to zero if
  -- you want data to be written to disk directly.
  , storeInterval :: Int
  -- ^ Default interval when creating a new counter.
  }

newStore :: Maybe FilePath -> IO Store
newStore root = do
  r <- newIORef M.empty
  return $ Store r root 1440 60 -- TODO configuration

getCounter
  :: Store -- ^ Cache
  -> T.Text -- ^ Counter name
  -> IO Counter
getCounter Store{..} metric = do
  now <- (floor . toRational) <$> getPOSIXTime
  empty <- do
    r <- newIORef (now - (now `mod` storeInterval), [0], 1)
    return $ Counter r storeLimit storeInterval
  ref <- atomicModifyIORef storeCounters $ \ m ->
    case M.lookup metric m of
      Nothing -> let m' = M.insert metric empty m
                 in (m', empty)
      Just c -> (m, c)
  return ref

type Timestamp = Int

data Counter = Counter
  { cRef :: IORef (Timestamp, [Double], Int)
  -- ^ Last time the counter was updated (with cUpdated `mod` cInterval == 0),
  -- mutable list of values. Values are cons'd as they arrive. A
  -- normalization function will cons zeroes when the counter is not
  -- explicitely updated, and size of the list.
  , cLimit :: Int
  -- ^ Maximum number of values kept in the cache.
  , cInterval :: Int
  -- ^ How many seconds are covered by a single value.
  }

-- | This is a low-level function, provided for compatibility with
-- `Data.Whisper.Carbon`. Functions specific to counters and gauges
-- should be prefered.
push :: Store -> T.Text -> Timestamp -> Double -> IO ()
push cache metric t value = do
  counter <- getCounter cache metric -- TODO not necessarily a counter.
  adjustCounter t counter (const value)

-- | Write a value for the given metric name. As it is a gauge, the value is
-- considered to be the same the next time slot if it is not explicitely updated.
set :: Store -> T.Text -> Double -> IO ()
set cache metric value = undefined

-- | Write a value for the given metric name. As it is a counter, the value is
-- considered to be zero at the beginning of the next time slot.
inc :: Store -> T.Text -> IO ()
inc cache metric = do
  now <- floor . toRational <$> getPOSIXTime
  inct now cache metric

readCounter :: Store -> T.Text -> IO [Point]
readCounter cache metric = do
  Counter{..} <- getCounter cache metric
  (t, xs, l) <- readIORef cRef
  return . reverse $ zipWith Point [t, t - cInterval..] xs

readPoints :: Store -> T.Text -> IO [Point]
readPoints = readCounter


-- TODO Case when the cache is "pass-through", i.e. it must contain no value
-- (they are written to disc directly), and adjustCounterAndInc wouldn't work.
inct :: Timestamp -> Store -> T.Text -> IO ()
inct t cache metric = do
  counter <- getCounter cache metric
  adjustCounter t counter (+ 1)

-- | Add as many values (zeroes) as needed to cover all the cache interval
-- until `t`, and modify the current (`t`) value.
adjustCounter :: Timestamp -> Counter -> (Double -> Double) -> IO ()
adjustCounter t Counter{..} f = do
  !_ <- atomicModifyIORef cRef $ \(lastUpdate, xs, l) ->
    let n = (t - lastUpdate) `div` cInterval
        zeroes = replicate n 0
        (xs', l') = case zeroes ++ xs of
                [] -> ([f 0], 1) -- Does not happen (there is always at least
                               -- one value in the cache).
                (x:xs_) -> (f x : xs_, l + n)
        v = (t - (t `mod` cInterval), take cLimit xs', l + n)
    in (v, v)
  return ()
