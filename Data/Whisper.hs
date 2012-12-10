{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Whisper
  ( closeWhisper, openWhisper, readMetaData
  , MetaData(..)
  ) where

import Control.Exception (finally)
import Data.Word (Word8, Word32)
import Data.Storable.Endian (peekBE, pokeBE)
import Foreign.C.Types (CFloat)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr, nullPtr, plusPtr, Ptr)
import Foreign.Storable
import System.IO.Posix.MMap.Internal (c_mmap, c_munmap)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix (closeFd, defaultFileFlags, fileSize, getFdStatus, openFd, OpenMode(ReadOnly))
import System.Posix.Types (Fd)

data Whisper = Whisper { whisperPtr :: Ptr Word8, whisperFd :: Fd }

openWhisper :: FilePath -> IO Whisper
openWhisper path = do
  fd <- openFd path ReadOnly Nothing defaultFileFlags
  flip finally (closeFd fd) $ do
    stat <- getFdStatus fd
    let size = fromIntegral (fileSize stat)
    ptr <- c_mmap size (fromIntegral fd)
    if ptr == nullPtr
      then error "Data.Whisper.openWhisper: unable to mmap file."
      else return $ Whisper ptr fd

closeWhisper :: Whisper -> IO ()
closeWhisper (Whisper ptr fd) = do
  _ <- c_munmap ptr (fromIntegral fd) -- TODO check error code
  return ()

data MetaData = MetaData
  { mdAggregationType :: Word32
  , mdMaxRetention :: Word32
  , mdXFilesFactor :: CFloat
  , mdArchiveCount :: Word32
  }
  deriving Show

instance Storable MetaData where
  sizeOf MetaData{..} =
    sizeOf mdAggregationType
    + sizeOf mdMaxRetention
    + sizeOf mdXFilesFactor
    + sizeOf mdArchiveCount

  alignment _ = alignment (undefined :: Word32)

  peek ptr = do
    a <- peekBE (castPtr ptr)
    b <- peekBE (castPtr ptr `plusPtr` 4)
    c <- coerce `fmap` peekBE (castPtr ptr `plusPtr` 8)
    d <- peekBE (castPtr ptr `plusPtr` 12)
    return $ MetaData a b c d
    where
    coerce :: Word32 -> CFloat
    coerce x = unsafePerformIO $ with x $ \p ->
      peek (castPtr p) :: IO CFloat

  poke ptr MetaData{..} = do
    pokeBE (castPtr ptr) mdAggregationType
    pokeBE (castPtr ptr `plusPtr` 4) mdMaxRetention
    pokeBE (castPtr ptr `plusPtr` 8) (coerce mdXFilesFactor)
    pokeBE (castPtr ptr `plusPtr` 12) mdArchiveCount
    where
    coerce :: CFloat -> Word32
    coerce x = unsafePerformIO $ with x $ \p ->
      peek (castPtr p) :: IO Word32

readMetaData :: Whisper -> IO MetaData
readMetaData Whisper{..} = peek (castPtr whisperPtr :: Ptr MetaData)
