{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Whisper
  ( closeWhisper, openWhisper, createWhisper
  , readHeader, readMetaData, readArchiveInfo
  , readArchive, readPoint
  , readArchives
  , aiRetention, aiSize
  , AggregationType(..)
  , Header(..), MetaData(..), ArchiveInfo(..), Archive(..), Point(..)
  ) where

import Prelude hiding (Enum, fromEnum)

import Control.Exception (finally)
import Data.Function (on)
import Data.List (sortBy)
import Data.Word (Word32, Word64)
import Data.Storable.Endian (peekBE, pokeBE)
import Foreign.C.Types
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr, nullPtr, plusPtr, Ptr)
import Foreign.Storable
import System.IO (SeekMode(..))
import System.IO.Unsafe (unsafePerformIO)
import System.Posix (closeFd, defaultFileFlags, fileSize, getFdStatus, openFd, OpenMode(..))
import System.Posix.IO (fdSeek, fdWrite)
import System.Posix.Files (ownerReadMode, ownerWriteMode, unionFileModes)
import System.Posix.Types (Fd)

-- TODO Some Int are used for unsigned long, which is not correct.
-- TODO Replace peekElemOff with plusPtr by peekByteOff.
-- TODO Use fcntl to lock the file.

#include <sys/mman.h>

class Enum a where
  toEnum :: Integer -> a
  fromEnum :: a -> Integer

#c
enum curved_MMAP_WRAPPER
{ curved_MAP_SHARED = MAP_SHARED
};
#endc

{# enum curved_MMAP_WRAPPER as MMAP_WRAPPER {} with prefix = "curved_" #}

#c
enum curved_PROT_WRAPPER
{ curved_PROT_READ = PROT_READ
, curved_PROT_WRITE = PROT_WRITE
};
#endc

{# enum curved_PROT_WRAPPER as PROT_WRAPPER {} with prefix = "curved_" #}

data Whisper = Whisper { whisperPtr :: Ptr (), whisperFd :: Fd }

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

-- | Open a memory-mapped file.
openWhisper :: FilePath -> IO Whisper
openWhisper path = do
  fd <- openFd path ReadWrite Nothing defaultFileFlags
  flip finally (closeFd fd) $ do -- TODO Can we close the fd before unmmap'ing it ?
    stat <- getFdStatus fd
    let size = fileSize stat
    ptr <- {# call mmap #} nullPtr (fromIntegral size) (fromInteger $ fromEnum PROT_READ) (fromInteger $ fromEnum MAP_SHARED) (fromIntegral fd) 0
    if ptr == nullPtr
      then error "Data.Whisper.openWhisper: unable to mmap file."
      else return $ Whisper (castPtr ptr) fd

closeWhisper :: Whisper -> IO ()
closeWhisper (Whisper ptr fd) = do
  _ <- {# call munmap #} ptr (fromIntegral fd) -- TODO check error code
  return ()

-- | Create a memory-mapped file of the given size.
openWhisperSize :: FilePath -> Int -> IO Whisper
openWhisperSize path size = do
  fd <- openFd path ReadWrite (Just $ ownerReadMode `unionFileModes` ownerWriteMode) defaultFileFlags
  -- Write a dummy byte to size correctly the underlying file befor
  -- memory-mapping it.
  fdSeek fd AbsoluteSeek (fromIntegral $ size - 1) >>= print --TODO check return value ? check size > 0 ?
  fdWrite fd "\0" >>= print
  flip finally (closeFd fd) $ do -- TODO Can we close the fd before unmmap'ing it ?
    ptr <- {# call mmap #} nullPtr (fromIntegral size) (fromInteger $ fromEnum PROT_WRITE) (fromInteger $ fromEnum MAP_SHARED) (fromIntegral fd) 0 -- TODO | PROT_READ
    print ptr
    if ptr == nullPtr
      then error "Data.Whisper.openWhisperSize: unable to mmap file."
      else return $ Whisper ptr fd

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
  ps <- mapM (readPoint w aiOffset) [0..aiPoints - 1]
  return $ Archive ps

readPoint :: Whisper -> Int -> Int -> IO Point
readPoint Whisper{..} archiveOffset n =
  peekElemOff (castPtr whisperPtr `plusPtr` archiveOffset) n

readArchives :: Whisper -> IO [Archive]
readArchives w = do
  Header{..} <- readHeader w
  mapM (readArchive w) [0..length hArchiveInfo - 1]

writeHeader :: Whisper -> Header -> IO ()
writeHeader w Header{..} = do
  writeMetaData w hMetaData
  mapM_ (uncurry $ writeArchiveInfo w) $ zip [0..] hArchiveInfo

writeMetaData :: Whisper -> MetaData -> IO ()
writeMetaData Whisper{..} = poke (castPtr whisperPtr)

writeArchiveInfo :: Whisper -> Int -> ArchiveInfo -> IO ()
writeArchiveInfo Whisper{..} n =
  pokeElemOff (castPtr whisperPtr `plusPtr` sizeOf (undefined :: MetaData)) n

writeArchive :: Whisper -> ArchiveInfo -> Archive -> IO ()
writeArchive w@Whisper{..} ArchiveInfo{..} (Archive points) =
  mapM_ (uncurry $ writePoint w aiOffset) $ zip [0..] points

writePoint :: Whisper -> Int -> Int -> Point -> IO ()
writePoint Whisper{..} archiveOffset n =
  pokeElemOff (castPtr whisperPtr `plusPtr` archiveOffset) n

writeArchives :: Whisper -> [ArchiveInfo] -> [Archive] -> IO ()
writeArchives w infos archives =
  mapM_ (uncurry $ writeArchive w) $ zip infos archives

createWhisper :: FilePath -> [(Int, Int)] -> Float -> AggregationType -> IO ()
createWhisper filename archiveInfos_ factor aggregation = do
  -- TODO archiveInfos_ can't be null.

  let archiveInfos = sortBy (compare `on` fst) archiveInfos_
      archiveInfos' = tail $ scanl toAI (ArchiveInfo (headerSize $ length archiveInfos) 1 0) archiveInfos
      retention = aiRetention $ last archiveInfos'
      meta = MetaData aggregation retention factor (length archiveInfos)
      header = Header meta archiveInfos'
      archives = map (Archive . flip replicate (Point 0 0) . snd) archiveInfos

  w@Whisper{..} <- openWhisperSize filename (headerSize (length archiveInfos) + sum (map snd archiveInfos) * sizeOf (undefined :: Point))
  writeHeader w header
  writeArchives w archiveInfos' archives
  closeWhisper w

-- | sizeOf a complete header, given the number of archives.
headerSize :: Int -> Int
headerSize n = sizeOf (undefined :: MetaData) + sizeOf (undefined :: ArchiveInfo) * n

-- | Given the precision and number of points of an archive (i.e. its
-- ArchiveInfo without its offset), and its predecessor archive, create a
-- complete ArchiveInfo (i.e. with its offset).
toAI :: ArchiveInfo -> (Int, Int) -> ArchiveInfo
toAI ArchiveInfo{..} (precision, points) =
  ArchiveInfo (aiOffset + sizeOf (undefined :: Point) * aiPoints) precision points

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
    sizeOf (undefined :: Word32)
    + sizeOf (undefined :: Word32)
    + sizeOf (undefined :: Word32)

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
