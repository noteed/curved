{-# LANGUAGE ScopedTypeVariables #-}
module Curved.Carbon where

import Control.Monad (forever, when)
import Control.Concurrent (forkIO)
import Data.List
import System.FilePath ((</>), (<.>))
import Network (accept, listenOn, withSocketsDo, PortID(..), Socket)
import System.Directory (doesFileExist)
import System.IO (hClose, hGetLine, hIsEOF, hPutStrLn, hSetBuffering, BufferMode(..), Handle)

import Data.Whisper

graphite_whisper_root = "/opt/graphite/storage/whisper"

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
              let path = foldl' (</>) graphite_whisper_root (split '.' metric) <.> "wsp"
              exist <- doesFileExist path
              when (not exist) $
                createWhisper path [(60, 1440)] 0.5 Average
              updateWhisperFile path timestamp value
              process handle
            _ -> hClose handle
        _ -> hClose handle

validMetricName metric = all (`elem` (['a'..'z'] ++ ['0'..'9'] ++ ".-")) metric
  && length metric > 0

split c s =
  case break (== c) s of
    (before, "") -> [before]
    (before, _ : after) -> before : split c after
