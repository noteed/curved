{-# LANGUAGE OverloadedStrings #-}
-- | Very partial implementation of the Python Pickle Virtual Machine: i.e.
-- parses pickled data into opcodes, then executes the opcodes to construct a
-- (Haskell representation of a) Python object.
module Data.Pickle where

import Control.Applicative ((<$>), (<*>), (*>))
import qualified Data.ByteString as S
import Data.Attoparsec hiding (take)
import qualified Data.Attoparsec as A
import Data.Attoparsec.Combinator
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import System.Environment (getArgs)

main :: IO ()
main = do
  [filename] <- getArgs
  content <- S.readFile filename
  print content

  case parseOnly (protocol_2 >> many1 opcodes) content of
    Left err -> error err
    Right xs -> do
      print xs
      print $ execute xs [] (IM.empty)

protocol_2 :: Parser ()
protocol_2 = string "\128\STX" *> return ()-- \x80\02, i.e. protocol 2

opcodes :: Parser OpCode
empty_dict :: Parser OpCode
binput, mark :: Parser OpCode
short_binstring :: Parser OpCode
setitem, setitems :: Parser OpCode
stop :: Parser OpCode

opcodes = choice
  [ empty_dict
  , binput, mark
  , short_binstring
  , setitem, setitems
  , stop
  ]

empty_dict = string "}" *> return EMPTY_DICT

binput = string "q" *> (BINPUT . fromIntegral <$> anyWord8)

mark = string "(" *> return MARK

short_binstring = do
  _ <- string "U"
  i <- fromIntegral <$> anyWord8
  s <- A.take i
  return $ SHORT_BINSTRING s

setitem = string "s" *> return SETITEM

setitems = string "u" *> return SETITEMS

stop = string "." *> return STOP

data OpCode =
    EMPTY_DICT
  | BINPUT Int
  | MARK
  | SHORT_BINSTRING S.ByteString
  | SETITEM
  | SETITEMS
  | STOP
  deriving Show

data Value =
    Dict (Map Value Value)
  | BinString S.ByteString
  | MarkObject -- Urk, not really a value.
  deriving (Eq, Ord, Show)

type Stack = [Value]

type Memo = IntMap Value

execute :: [OpCode] -> Stack -> Memo -> Either String Value
execute [] [value] memo = Right value
execute (op:ops) stack memo = case executeOne op stack memo of
  Left err -> Left err
  Right (stack', memo') -> execute ops stack' memo'
execute _ _ _ = Left "`execute` unimplemented"

executePartial [] stack memo = (stack, memo, [])
executePartial (op:ops) stack memo = case executeOne op stack memo of
  Left err -> (stack, memo, op:ops)
  Right (stack', memo') -> executePartial ops stack' memo'

executeOne :: OpCode -> Stack -> Memo -> Either String (Stack, Memo)
executeOne EMPTY_DICT stack memo = return (Dict M.empty: stack, memo)
executeOne (BINPUT i) (s:stack) memo = return (s:stack, IM.insert i s memo)
executeOne (SHORT_BINSTRING s) stack memo = return (BinString s:stack, memo)
executeOne MARK stack memo = return (MarkObject:stack, memo)
executeOne SETITEMS stack memo = executeSetitems [] stack memo
executeOne STOP stack memo = Right (stack, memo)
executeOne _ stack memo = Left "`executeOne` unimplemented"

executeSetitems l (MarkObject:Dict d:stack) memo = return (l `addToDict` Dict d:stack, memo)
executeSetitems l (a:b:stack) memo = executeSetitems ((b, a) : l) stack memo

addToDict l (Dict d) = Dict $ foldl' add d l
  where add d (a, b) = M.insert a b d
