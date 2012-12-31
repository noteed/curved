{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Garbage collection and memory statistics provided by `GHC.Stats`.
module Curved.RTS where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Stats

import Curved.Cache

pushGHCStatsToStore :: Store -> IO ()
pushGHCStatsToStore store = loop 0 0

  where
  -- The xxx_ are previous values, obtained in the previous iteration.
  -- The xxx' are the delta between previous an current values.
  loop mutatorWallSeconds_ gcWallSeconds_ = do
    now <- floor . toRational <$> liftIO getPOSIXTime
    GCStats{..} <- getGCStats

    let mutatorWallSeconds' = mutatorWallSeconds - mutatorWallSeconds_
    let gcWallSeconds' = gcWallSeconds - gcWallSeconds_

    -- TODO application-specific prefix
    push store "rts.memory" now (fromIntegral currentBytesUsed)
    push store "rts.mutator" now mutatorWallSeconds'
    push store "rts.gc" now gcWallSeconds'
    push store "rts.productivity" now $
      mutatorWallSeconds' / (mutatorWallSeconds' + gcWallSeconds') * 100.0

    -- Sleep just enough to reach the next metric point (assuming a 60 seconds
    -- step). TODO make sure the assumption is always correct.
    let nextStep = now - (now `mod` 60) + 60
    threadDelay $ (nextStep - now) * 1000000
    loop mutatorWallSeconds gcWallSeconds
