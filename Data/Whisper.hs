{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Whisper
  ( closeWhisper, openWhisper
  , readHeader, readMetaData, readArchiveInfo
  , Header(..), MetaData(..), ArchiveInfo(..)
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

data Header = Header
  { hMetaData :: MetaData
  , hArchiveInfo :: [ArchiveInfo]
  }
  deriving Show

data MetaData = MetaData
  { mdAggregationType :: Word32
  , mdMaxRetention :: Word32
  , mdXFilesFactor :: CFloat
  , mdArchiveCount :: Word32
  }
  deriving Show

data ArchiveInfo = ArchiveInfo
  { aiOffset :: Word32
  , aiSecondsPerPoint :: Word32
  , aiPoints :: Word32
  }
  deriving Show

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

readHeader :: Whisper -> IO Header
readHeader w = do
  md <- readMetaData w
  ai <- readArchiveInfo w (fromIntegral $ mdArchiveCount md)
  return $ Header md ai

readMetaData :: Whisper -> IO MetaData
readMetaData Whisper{..} = peek (castPtr whisperPtr)

-- TODO replace peekElemOff with plusPtr by peekByteOff
readArchiveInfo :: Whisper -> Int -> IO [ArchiveInfo]
readArchiveInfo Whisper{..} 0 = return []
readArchiveInfo Whisper{..} n = mapM
  (peekElemOff (castPtr whisperPtr `plusPtr` sizeOf (undefined :: MetaData))) [0.. n - 1]

instance Storable MetaData where
  sizeOf _ =
    sizeOf (mdAggregationType undefined)
    + sizeOf (mdMaxRetention undefined)
    + sizeOf (mdXFilesFactor undefined)
    + sizeOf (mdArchiveCount undefined)

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

instance Storable ArchiveInfo where
  sizeOf _ =
    sizeOf (aiOffset undefined)
    + sizeOf (aiSecondsPerPoint undefined)
    + sizeOf (aiPoints undefined)

  alignment _ = alignment (undefined :: Word32)

  peek ptr = do
    a <- peekBE (castPtr ptr)
    b <- peekBE (castPtr ptr `plusPtr` 4)
    c <- peekBE (castPtr ptr `plusPtr` 8)
    return $ ArchiveInfo a b c

  poke ptr ArchiveInfo{..} = do
    pokeBE (castPtr ptr) aiOffset
    pokeBE (castPtr ptr `plusPtr` 4) aiSecondsPerPoint
    pokeBE (castPtr ptr `plusPtr` 8) aiPoints
