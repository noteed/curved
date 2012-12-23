{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Very partial implementation of the Python Pickle Virtual Machine
-- (protocol 2): i.e. parses pickled data into opcodes, then executes the
-- opcodes to construct a (Haskell representation of a) Python object.

module Data.Pickle where

import Control.Applicative ((<$>), (<*>), (*>))
import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.ByteString as S
import Data.Attoparsec hiding (take)
import qualified Data.Attoparsec as A
import Data.Attoparsec.Combinator
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Serialize.Put (runPut, putByteString, putWord8, Put)

-- Note \128 is the same as \STX\x80\02, i.e. protocol 2.

unpickle :: S.ByteString -> Either String Value
unpickle s = do
  xs <- parseOnly (string "\128\STX" *> many1 opcodes) s
  unpickle' xs

pickle :: Value -> S.ByteString
pickle value = runPut $ do
  putByteString "\128\STX"
  mapM_ serialize . runPickler $ pickle' value
  putByteString "."

----------------------------------------------------------------------
-- Pickle opcodes parser
----------------------------------------------------------------------

-- Maybe parsing could be done with a big switch and only cereal,
-- instead of relying on attoparsec's "choice" combinator.

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

----------------------------------------------------------------------
-- Pickle opcodes serialization
----------------------------------------------------------------------

serialize opcode = case opcode of
  BINPUT i -> putByteString "q" >> putWord8 (fromIntegral i)
  SHORT_BINSTRING s -> do
    putByteString "U"
    putWord8 . fromIntegral $ S.length s
    putByteString s
  EMPTY_DICT -> putByteString "}"
  MARK -> putByteString "("
  SETITEMS -> putByteString "u"

----------------------------------------------------------------------
-- Pickle opcodes
----------------------------------------------------------------------

data OpCode =
    EMPTY_DICT
  | BINPUT Int
  | MARK
  | SHORT_BINSTRING S.ByteString
  | SETITEM
  | SETITEMS
  | STOP
  deriving Show

----------------------------------------------------------------------
-- Pyhon value representation
----------------------------------------------------------------------

-- Maybe I can call them Py? And Have IsString/Num instances?
data Value =
    Dict (Map Value Value)
  | BinString S.ByteString
  | MarkObject -- Urk, not really a value.
  deriving (Eq, Ord, Show)

----------------------------------------------------------------------
-- Pickle machine (opcodes to value)
----------------------------------------------------------------------

unpickle' :: [OpCode] -> Either String Value
unpickle' xs = execute xs [] (IM.empty)

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

----------------------------------------------------------------------
-- Pickling (value to opcodes)
----------------------------------------------------------------------

newtype Pickler a = Pickler { runP :: WriterT [OpCode] (State (Map Value Int)) a }
  deriving (Monad, MonadWriter [OpCode], MonadState (Map Value Int))

runPickler :: Pickler () -> [OpCode]
runPickler p = evalState (execWriterT (runP p)) M.empty

pickle' :: Value -> Pickler ()
pickle' value = case value of
  Dict d -> pickleDict d
  BinString s -> pickleBinString s
  x -> error $ "TODO: pickle " ++ show x

-- TODO actually lookup values in the map, reusing their key.
binput' :: Value -> Pickler ()
binput' value = do
  i <- gets M.size
  m <- get
  put (M.insert value i m)
  tell [BINPUT i]

pickleDict :: Map Value Value -> Pickler ()
pickleDict d = do
  tell [EMPTY_DICT]
  binput' (Dict d)

  let kvs = M.toList d
  when (not $ null kvs) $ do
    tell [MARK]
    mapM_ (\(k, v) -> pickle' k >> pickle' v) kvs
    tell [SETITEMS]

-- TODO depending on the string length, it should not always be a SHORT_BINSTRING
pickleBinString :: S.ByteString -> Pickler ()
pickleBinString s = do
  tell [SHORT_BINSTRING s]
  binput' (BinString s)

----------------------------------------------------------------------
-- Manipulate Values
----------------------------------------------------------------------

dictGet :: Value -> Value -> Either String (Maybe Value)
dictGet (Dict d) v = return $ M.lookup v d
dictGet _ _ = Left "dictGet: not a dict."

dictGet' :: Value -> Value -> Either String Value
dictGet' (Dict d) v = case M.lookup v d of
  Just value -> return value
  Nothing -> Left "dictGet': no such key."
dictGet' _ _ = Left "dictGet': not a dict."

dictGetString :: Value -> S.ByteString -> Either String S.ByteString
dictGetString (Dict d) s = case M.lookup (BinString s) d of
  Just (BinString s') -> return s'
  _ -> Left "dictGetString: not a dict, or no such key."
