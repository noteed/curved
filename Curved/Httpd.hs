{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Curved.Httpd where

import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Text as T

import Snap
import Snap.Util.FileServe (serveDirectory, serveFile)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.Whisper
import Data.Whisper.Store
-- import Curved.Cache # TODO Use a cache, after benchmarks are done.

data App = App

makeLenses [''App]

httpd :: Store -> Int -> ByteString -> IO ()
httpd store port host = do
#ifdef DEV
  putStrLn "-----------------------------"
  putStrLn "THIS IS A DEVELOPMENT VERSION"
  putStrLn "-----------------------------"
#endif
  let config = setHostname host $ setPort port $
               setVerbose True defaultConfig
  serveSnaplet config (appInit store)

appInit :: Store -> SnapletInit App App
appInit store = makeSnaplet "curved-web-server" "Curved Web Server" Nothing $ do
  addRoutes $ routes store
  wrapSite (<|> serveFile "site/metric.html")
  return App

routes :: Store -> [(ByteString, Handler App App ())]
routes store =
  [ ("/", serveDirectory "site")
  , ("/csv/sin", sinCsv)
  , serveWhisperAsCsv store "/csv/"
  ]

sinCsv :: Handler App App ()
sinCsv = do
  now <- floor . toRational <$> liftIO getPOSIXTime
  let dat = concat . map l . zip [now-299..(now::Int)] $
        map (sin . (/ 10) . fromIntegral) [now..]
      l (a, b) = show a ++ "," ++ show (b::Double) ++ "\n"
  writeBS $ "date,close\n" `B.append` B.pack dat

dataCsv :: Store -> String -> Handler App App () -- TODO no need to pass explicitely Store around, can be get from the state monad.
dataCsv store metric = do
  points <- liftIO $ do readPoints store metric

  let dat = concat $ map l $ filter f points
      f (Point a _) = a /= 0
      l (Point a b) = show a ++ "," ++ show b ++ "\n"
  writeBS $ "date,close\n" `B.append` B.pack dat

serveWhisperAsCsv :: Store -> ByteString -> (ByteString, Handler App App ())
serveWhisperAsCsv store prefix = (B.append prefix ":whatever",) $ do
  metric <- (B.drop (B.length prefix) . B.takeWhile (/= '?') . rqURI) <$> getRequest
  dataCsv store (map (\c -> if c == '/' then '.' else c) $ B.unpack metric)

