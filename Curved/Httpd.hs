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

import Control.Applicative ((<$>))
import Data.Time.Clock.POSIX (getPOSIXTime)

import Snap
import Snap.Snaplet (wrapHandlers)
import qualified Snap.Blaze as Blaze (blaze)
import Snap.Util.FileServe (serveDirectory, serveFile)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.Whisper

data App = App

makeLenses [''App]

httpd :: Int -> ByteString -> IO ()
httpd port host = do
#ifdef DEV
  putStrLn "-----------------------------"
  putStrLn "THIS IS A DEVELOPMENT VERSION"
  putStrLn "-----------------------------"
#endif
  let config = setHostname host $ setPort port $
               setVerbose True defaultConfig
  serveSnaplet config appInit

appInit :: SnapletInit App App
appInit = makeSnaplet "curved-web-server" "Curved Web Server" Nothing $ do
  addRoutes routes
  wrapHandlers (<|> serveFile "site/metric.html")
  return App

routes :: [(ByteString, Handler App App ())]
routes =
  [ ("/", serveDirectory "site")
  , ("/csv/sin", sinCsv)
  , serveWhisperAsCsv "/csv/" "/opt/graphite/storage/whisper/"
  ]

sinCsv :: Handler App App ()
sinCsv = do
  now_ <- liftIO getPOSIXTime
  let now = floor $ toRational now_
      dat = concat $ map l (zip [now-299..now] $ map (sin . (/ 10) . fromIntegral) [now..])
      l (a, b) = show a ++ "," ++ show b ++ "\n"
  writeBS $ "date,close\n" `B.append` B.pack dat

dataCsv :: FilePath -> Handler App App ()
dataCsv filename = do
  points <- liftIO $ do
    wsp <- openWhisper filename
    now <- (floor . toRational) <$> liftIO getPOSIXTime
    Archive points <- readArchive wsp 0 now
    closeWhisper wsp
    return points

  let dat = concat $ map l $ filter f points
      f (Point a _) = a /= 0
      l (Point a b) = show a ++ "," ++ show b ++ "\n"
  writeBS $ "date,close\n" `B.append` B.pack dat

--serveWhisperAsCsv :: FilePath -> Handler App App ()
serveWhisperAsCsv prefix dir = (B.append prefix ":whatever",) $ do
  metric <- (B.drop (B.length prefix) . B.takeWhile (/= '?') . rqURI) <$> getRequest
  dataCsv $ dir ++ B.unpack metric ++ ".wsp"

