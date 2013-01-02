{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Curved.Carbon where

import Control.Applicative ((<$>))
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import qualified Data.Map as M
import Data.Serialize.Get (getWord32be, runGet)
import Data.Serialize.Put (putWord32be, runPut)
import qualified Data.Text as T
import Language.Python.Pickle (dictGetString, pickle, unpickle, Value(..))
import Network (accept, listenOn, PortID(..), PortNumber, Socket)
import qualified Network.Socket as N (accept, sClose)
import qualified Network.Socket.ByteString as NS
import System.IO (hClose, hGetLine, hIsEOF, hSetBuffering, BufferMode(..), Handle)

import Data.Whisper
import Curved.Cache (push, readPoints, Store)

receivePoints :: Store -> PortNumber -> IO ()
receivePoints store port = do
  sock <- listenOn $ PortNumber port
  forever $ do
    (handle, _, _) <- accept sock
    -- hSetBuffering handle NoBuffering
    forkIO $ processPoints store handle

processPoints :: Store -> Handle -> IO ()
processPoints store handle = do
  eof <- hIsEOF handle
  if eof
    then hClose handle
    else do
      line <- hGetLine handle
      case words line of
        [metric, value_, timestamp_] ->
          case (validMetricName metric, reads value_, reads timestamp_) of
            (True, [(value :: Double, "")], [(timestamp :: Int, "")]) -> do
              push store (T.pack metric) timestamp value
              processPoints store handle
            _ -> hClose handle
        _ -> hClose handle

validMetricName :: String -> Bool
validMetricName metric = all (`elem` (['a'..'z'] ++ ['0'..'9'] ++ ".-")) metric
  && length metric > 0

receiveQueries :: Store -> PortNumber -> IO ()
receiveQueries store port = do
  sock <- listenOn $ PortNumber port
  forever $ do
    (sock', _) <- N.accept sock
    -- hSetBuffering handle NoBuffering
    forkIO $ processQueries store sock'

-- Loop on the same handle: the client can issue
-- multiple requests on the same connection.
processQueries :: Store -> Socket -> IO ()
processQueries store sock = do
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
              response <- processQuery store content
              case response of
                Left err -> N.sClose sock >> print err -- TODO log corrupt queries
                Right r -> NS.send sock r >> processQueries store sock

data Query =
    CacheQuery S.ByteString -- ^ Query the cache for a specific metric name.
  deriving Show

processQuery :: Store -> S.ByteString -> IO (Either String S.ByteString)
processQuery store s = case parseQuery s of
  Left err -> return $ Left err
  Right query -> do
    response <- processQuery' store query
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

processQuery' :: Store -> Query -> IO Value
processQuery' store (CacheQuery metric) = do
  datapoints <- readPoints store (T.pack $ SC.unpack metric)
  return . Dict $ M.fromList[(BinString "datapoints", List $ tuple datapoints)]
  where
  tuple = map (\(Point a b) -> Tuple [BinInt a, BinFloat b])

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
    
