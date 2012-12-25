{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Curved.Carbon where

import Control.Applicative ((<$>))
import Control.Monad (forever, when)
import Control.Concurrent (forkIO)
import qualified Data.ByteString as S
import qualified Data.Map as M
import Data.Serialize.Get (getWord32be, runGet)
import Data.Serialize.Put (putWord32be, runPut)
import Data.List
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network (accept, listenOn, PortID(..), PortNumber, Socket)
import qualified Network.Socket as N (accept, sClose)
import qualified Network.Socket.ByteString as NS
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))
import System.IO (hClose, hGetLine, hIsEOF, hSetBuffering, BufferMode(..), Handle)

import Data.Pickle
import Data.Whisper

graphite_whisper_root :: FilePath
graphite_whisper_root = "/opt/graphite/storage/whisper"

receivePoints :: PortNumber -> IO ()
receivePoints port = do
  sock <- listenOn $ PortNumber port
  forever $ do
    (handle, _, _) <- accept sock
    -- hSetBuffering handle NoBuffering
    forkIO $ processPoints handle

processPoints :: Handle -> IO ()
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

validMetricName :: String -> Bool
validMetricName metric = all (`elem` (['a'..'z'] ++ ['0'..'9'] ++ ".-")) metric
  && length metric > 0

split :: Eq a => a -> [a] -> [[a]]
split c s =
  case break (== c) s of
    (before, []) -> [before]
    (before, _ : after) -> before : split c after

receiveQueries :: PortNumber -> IO ()
receiveQueries port = do
  sock <- listenOn $ PortNumber port
  forever $ do
    (sock', _) <- N.accept sock
    -- hSetBuffering handle NoBuffering
    forkIO $ processQueries sock'

-- Loop on the same handle: the client can issue
-- multiple requests on the same connection.
processQueries :: Socket -> IO ()
processQueries sock = do
  ms <- receive sock 4
  case ms of
    Nothing -> N.sClose sock -- TODO log corrupt queries
    Just s -> do
      let esize = runGet getWord32be s
      case esize of
        -- TODO is it possible to have a Left here ?
        Left _ -> N.sClose sock
        Right size -> do
          mcontent <- receive sock $ fromIntegral size
          case mcontent of
            Nothing -> N.sClose sock -- TODO log corrupt queries
            Just content -> do
              response <- processQuery content
              case response of
                Left err -> N.sClose sock >> print err -- TODO log corrupt queries
                Right r -> NS.send sock r >> processQueries sock

data Query =
    CacheQuery S.ByteString -- ^ Query the cache for a specific metric name.
  deriving Show

processQuery :: S.ByteString -> IO (Either String S.ByteString)
processQuery s = case parseQuery s of
  Left err -> return $ Left err
  Right query -> do
    response <- processQuery' query
    let r = pickle response
    return . Right $
      runPut (putWord32be . fromIntegral $ S.length r) `S.append` r

parseQuery :: S.ByteString -> Either String Query
parseQuery s = do
  value <- unpickle s
  typ <- value `dictGetString` "type"
  case typ of
    "cache-query" -> CacheQuery <$> value `dictGetString` "metric"
    x -> Left $ "parseQuery: unknown query type." ++ show x

processQuery' :: Query -> IO Value
processQuery' (CacheQuery metric) = do
  -- now <- (floor . toRational) <$> getPOSIXTime
  -- let datapoints = zip (reverse $ take 1440 [now, now-60..]) (map sin [0,0.05..])
  datapoints <- return [] -- TODO Get the points from an actual cache.
  return . Dict $ M.fromList[(BinString "datapoints", List $ tuple datapoints)]
  where
  tuple = map (\(a, b) -> Tuple [BinInt a, BinFloat b])

receive :: Socket -> Int -> IO (Maybe S.ByteString)
receive sock n = loop 0 []
  where
  bufferSize = 4096
  loop alreadyRead ss = do
    let r = min (n - alreadyRead) bufferSize
    s <- NS.recv sock r
    if S.length s + alreadyRead == n
      then return . Just . S.concat $ reverse (s : ss)
      else if S.length s == r
             then loop (alreadyRead + S.length s) (s : ss)
             else return Nothing
    
