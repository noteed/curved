-- Push the load average (read from /proc/loadavg) to graphite (i.e. to the
-- carbon-cache process in a normal graphite setup).
-- Code similar to the graphite-client.py example program.
module Main (main) where

import Control.Applicative ((<$>))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Socket
import Network.BSD
import Data.List
import System.Environment (getArgs)
import System.IO

-- carbon-cache server:
-- hostname = "127.0.0.1"
-- port = "2003"

main :: IO ()
main = do
  [hostname, port] <- getArgs
  now <- (show . floor . toRational) <$> getPOSIXTime
  content <- readFile "/proc/loadavg"
  putStr content

  -- TODO explosive constructs
  let average1 = (read . (!! 0) $ words content) :: Float
      average5 = (read . (!! 1) $ words content) :: Float
      average15 = (read . (!! 2) $ words content) :: Float

  addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  h <- socketToHandle sock WriteMode

  hPutStrLn h $ "system.load-average.01min " ++ show average1 ++ " " ++ now
  hPutStrLn h $ "system.load-average.05min " ++ show average5 ++ " " ++ now
  hPutStrLn h $ "system.load-average.15min " ++ show average15 ++ " " ++ now

  hClose h
