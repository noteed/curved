module Main (main) where

import System.Environment (getArgs)

import Data.Whisper

main :: IO ()
main = do
  [filename] <- getArgs
  wsp <- openWhisper filename
  header <- readHeader wsp
  print header
  closeWhisper wsp
