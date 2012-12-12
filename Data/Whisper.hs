{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Whisper
  ( closeWhisper, openWhisper
  , readHeader, readMetaData, readArchiveInfo
  , readArchive, readPoint
  , readArchives
  , aiRetention, aiSize
  , Header(..), MetaData(..), ArchiveInfo(..), Archive(..), Point(..)
  ) where

import Control.Exception (finally)
import Data.Word (Word8, Word32, Word64)
import Data.Storable.Endian (peekBE, pokeBE)
import Foreign.C.Types (CDouble, CFloat)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr, nullPtr, plusPtr, Ptr)
import Foreign.Storable
import System.IO.Posix.MMap.Internal (c_mmap, c_munmap)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix (closeFd, defaultFileFlags, fileSize, getFdStatus, openFd, OpenMode(ReadOnly))
import System.Posix.Types (Fd)

-- TODO Some Int are used for unsigned long, which is not correct.
-- TODO Replace peekElemOff with plusPtr by peekByteOff.

data Whisper = Whisper { whisperPtr :: Ptr Word8, whisperFd :: Fd }

data Header = Header
  { hMetaData :: MetaData
  , hArchiveInfo :: [ArchiveInfo]
  }
  deriving Show

data MetaData = MetaData
  { mdAggregationType :: AggregationType
  , mdMaxRetention :: Int
  , mdXFilesFactor :: Float
  , mdArchiveCount :: Int
  }
  deriving Show

data ArchiveInfo = ArchiveInfo
  { aiOffset :: Int
  , aiSecondsPerPoint :: Int
  , aiPoints :: Int
  }
  deriving Show

aiRetention :: ArchiveInfo -> Int
aiRetention ArchiveInfo{..} = aiSecondsPerPoint * aiPoints

aiSize :: ArchiveInfo -> Int
aiSize ArchiveInfo{..} = aiPoints * sizeOf (undefined :: Point)

data AggregationType = Average | Sum | Last | Max | Min
  deriving Show

data Archive = Archive [Point]
  deriving Show

data Point = Point Int Double
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
  ai <- mapM (readArchiveInfo w) [0..fromIntegral (mdArchiveCount md) - 1]
  return $ Header md ai

readMetaData :: Whisper -> IO MetaData
readMetaData Whisper{..} = peek (castPtr whisperPtr)

readArchiveInfo :: Whisper -> Int -> IO ArchiveInfo
readArchiveInfo Whisper{..} n =
  peekElemOff (castPtr whisperPtr `plusPtr` sizeOf (undefined :: MetaData)) n

readArchive :: Whisper -> Int -> IO Archive
readArchive w@Whisper{..} n = do
  ArchiveInfo{..} <- readArchiveInfo w n
  print (aiOffset, aiPoints)
  ps <- mapM (readPoint w aiOffset) [0..aiPoints - 1]
  return $ Archive ps

readPoint :: Whisper -> Int -> Int -> IO Point
readPoint Whisper{..} archive n = do
  p <- peekElemOff (castPtr whisperPtr `plusPtr` archive) n
  print p
  return p

readArchives :: Whisper -> IO [Archive]
readArchives w = do
  Header{..} <- readHeader w
  mapM (readArchive w) [0..length hArchiveInfo - 1]

instance Storable MetaData where
  sizeOf _ =
    sizeOf (undefined :: Word32)
    + sizeOf (undefined :: Word32)
    + sizeOf (undefined :: CFloat)
    + sizeOf (undefined :: Word32)

  alignment _ = alignment (undefined :: Word32)

  peek ptr = do
    a <- peek (castPtr ptr)
    b <- fromIntegral `fmap` peekBE (castPtr ptr `plusPtr` 4 :: Ptr Word32)
    c <- coerce `fmap` peekBE (castPtr ptr `plusPtr` 8)
    d <- fromIntegral `fmap` peekBE (castPtr ptr `plusPtr` 12 :: Ptr Word32)
    return $ MetaData a b c d
    where
    coerce :: Word32 -> Float
    coerce x = unsafePerformIO $ with x $ \p ->
      peek (castPtr p) :: IO Float

  poke ptr MetaData{..} = do
    poke (castPtr ptr) mdAggregationType
    pokeBE (castPtr ptr `plusPtr` 4) (fromIntegral mdMaxRetention :: Word32)
    pokeBE (castPtr ptr `plusPtr` 8) (coerce mdXFilesFactor)
    pokeBE (castPtr ptr `plusPtr` 12) (fromIntegral mdArchiveCount :: Word32)
    where
    coerce :: Float -> Word32
    coerce x = unsafePerformIO $ with x $ \p ->
      peek (castPtr p) :: IO Word32

instance Storable ArchiveInfo where
  sizeOf _ =
    sizeOf (aiOffset undefined)
    + sizeOf (aiSecondsPerPoint undefined)
    + sizeOf (aiPoints undefined)

  alignment _ = alignment (undefined :: Word32)

  peek ptr = do
    a <- fromIntegral `fmap` peekBE (castPtr ptr :: Ptr Word32)
    b <- fromIntegral `fmap` peekBE (castPtr ptr `plusPtr` 4 :: Ptr Word32)
    c <- fromIntegral `fmap` peekBE (castPtr ptr `plusPtr` 8 :: Ptr Word32)
    return $ ArchiveInfo a b c

  poke ptr ArchiveInfo{..} = do
    pokeBE (castPtr ptr) (fromIntegral aiOffset :: Word32)
    pokeBE (castPtr ptr `plusPtr` 4) (fromIntegral aiSecondsPerPoint :: Word32)
    pokeBE (castPtr ptr `plusPtr` 8) (fromIntegral aiPoints :: Word32)

instance Storable AggregationType where
  sizeOf _ = sizeOf (undefined :: Word32)

  alignment _ = alignment (undefined :: Word32)

  peek ptr = agg `fmap` peekBE (castPtr ptr :: Ptr Word32)
    where
    agg 1 = Average
    agg 2 = Sum
    agg 3 = Last
    agg 4 = Max
    agg 5 = Min
    agg _ = Average

  poke ptr a = pokeBE (castPtr ptr) $ agg a
    where
    agg Average = 1 :: Word32
    agg Sum = 2
    agg Last = 3
    agg Max = 4
    agg Min = 5

instance Storable Point where
  sizeOf _ = sizeOf (undefined :: Word32)
    + sizeOf (undefined :: CDouble)

  alignment _ = alignment (undefined :: Word32)

  peek ptr = do
    a <- fromIntegral `fmap` peekBE (castPtr ptr :: Ptr Word32)
    b <- coerce `fmap` peekBE (castPtr ptr `plusPtr` 4)
    return $ Point a b
    where
    coerce :: Word64 -> Double
    coerce x = unsafePerformIO $ with x $ \p ->
      peek (castPtr p) :: IO Double

  poke ptr (Point a b) = do
    pokeBE (castPtr ptr) (fromIntegral a :: Word32)
    pokeBE (castPtr ptr `plusPtr` 4) (coerce b)
    where
    coerce :: Double -> Word64
    coerce x = unsafePerformIO $ with x $ \p ->
      peek (castPtr p) :: IO Word64
