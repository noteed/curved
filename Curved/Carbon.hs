{-# LANGUAGE ScopedTypeVariables #-}
module Curved.Carbon where

import Control.Monad (forever, when)
import Control.Concurrent (forkIO)
import Network (accept, listenOn, withSocketsDo, PortID(..), Socket)
import System.Directory (doesFileExist)
import System.IO (hClose, hGetLine, hIsEOF, hPutStrLn, hSetBuffering, BufferMode(..), Handle)

import Data.Whisper

receivePoints port = do
  sock <- listenOn $ PortNumber port
  forever $ do
    (handle, _, _) <- accept sock
    -- hSetBuffering handle NoBuffering
    forkIO $ process handle

process handle = do
  eof <- hIsEOF handle
  if eof
    then hClose handle
    else do
      line <- hGetLine handle
      putStrLn $ "Received: " ++ line
      case words line of
        [metric, value_, timestamp_] ->
          case (validMetricName metric, reads value_, reads timestamp_) of
            (True, [(value :: Double, "")], [(timestamp :: Int, "")]) -> do
              exist <- doesFileExist $ metric ++ ".wsp"
              when (not exist) $
                createWhisper (metric ++ ".wsp") [(60, 1440)] 0.5 Average
              updateWhisperFile (metric ++ ".wsp") timestamp value
              process handle
            _ -> hClose handle
        _ -> hClose handle

validMetricName = all (`elem` (['a'..'z'] ++ ['0'..'9'] ++ ".-"))
