module Main (main) where

import System.Environment (getArgs)

import Data.Whisper

main :: IO ()
main = do
  [filename] <- getArgs
  wsp <- openWhisper filename
  metaData <- readMetaData wsp
  print metaData
  closeWhisper wsp
