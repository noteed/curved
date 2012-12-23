module Main (main) where

import qualified Data.ByteString as S
import System.Environment (getArgs)

import Data.Pickle

main :: IO ()
main = do
  [filename] <- getArgs
  content <- S.readFile filename
  print content
  print $ unpickle content
