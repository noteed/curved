{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Curved.Carbon where

import Control.Applicative ((<$>))
import Control.Monad (forever, when)
import Control.Concurrent (forkIO)
import qualified Data.ByteString as S
import Data.Serialize.Get (getWord32be, runGet)
import Data.List
import System.FilePath ((</>), (<.>))
import Network (accept, listenOn, withSocketsDo, PortID(..), Socket)
import System.Directory (doesFileExist)
import System.IO (hClose, hGetLine, hIsEOF, hPutStrLn, hSetBuffering, BufferMode(..), Handle)

import Data.Pickle
import Data.Whisper

graphite_whisper_root = "/opt/graphite/storage/whisper"

receivePoints port = do
  sock <- listenOn $ PortNumber port
  forever $ do
    (handle, _, _) <- accept sock
    -- hSetBuffering handle NoBuffering
    forkIO $ processPoints handle

processPoints handle = do
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
              processPoints handle
            _ -> hClose handle
        _ -> hClose handle

validMetricName metric = all (`elem` (['a'..'z'] ++ ['0'..'9'] ++ ".-")) metric
  && length metric > 0

split c s =
  case break (== c) s of
    (before, "") -> [before]
    (before, _ : after) -> before : split c after

receiveQueries port = do
  sock <- listenOn $ PortNumber port
  forever $ do
    (handle, _, _) <- accept sock
    -- hSetBuffering handle NoBuffering
    forkIO $ processQueries handle

processQueries handle = do
  content <- S.hGetContents handle -- handle is closed by hGetContents.
  print $ parseQuery content

parseQuery s = do
  -- The query is a bytestring prefixed by its length (as a network
  -- byte-order int32).
  -- We don't really need the prefix as we read everything at once
  -- but we use it as a check.
  size <- runGet getWord32be s
  value <- if fromIntegral size == S.length s - 4
             then unpickle (S.drop 4 s)
             else Left "parseQuery: advertized size doesn't match message length."
  typ <- value `dictGetString` "type"
  case typ of
    "cache-query" -> CacheQuery <$> value `dictGetString` "metric"
    x -> Left $ "parseQuery: unknown query type." ++ show x

data Query =
    CacheQuery S.ByteString -- ^ Query the cache for a specific metric name.
  deriving Show
