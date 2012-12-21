{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Whisper
  ( closeWhisper, openWhisper, createWhisper
  , readHeader, readMetaData, readArchiveInfo
  , readArchive, readPoint
  , readArchives
  , updateWhisper
  , aiRetention, aiSize
  , AggregationType(..)
  , Header(..), MetaData(..), ArchiveInfo(..), Archive(..), Point(..)
  ) where

import Prelude hiding (Enum, fromEnum)

import Control.Applicative ((<$>), (<*>))
import Control.Exception (finally)
import Data.Bits ((.|.))
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
-- TODO Probably the header can be directly available in the Whisper data type
-- (probably it is almost always necessary to know the complete file layout (as
-- provided by the ArchiveInfo) to do any meaningful operation).

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
    ptr <- {# call mmap #} nullPtr (fromIntegral size) (fromInteger $ fromEnum PROT_READ .|. fromEnum PROT_WRITE) (fromInteger $ fromEnum MAP_SHARED) (fromIntegral fd) 0
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

-- | Read the nth archive, considering it as a window ending at `timestamp`.
readArchive :: Whisper -> Int -> Timestamp -> IO Archive
readArchive w@Whisper{..} n timestamp = do
  ai@ArchiveInfo{..} <- readArchiveInfo w n -- TODO the archiveInfo is usually already read.
  if aiPoints <= 0
    then return $ Archive []
    else do
      -- The very first point presence indicates if data are present.
      Point ts _ <- readPoint w aiOffset 0
      if ts == 0
        then return $ Archive []
        else do
          -- The first point in the archive is used as a base point
          -- to index into the archive data.
          ps <- mapM (readPoint w aiOffset) [0..aiPoints - 1]
          let (after, before) = splitAt (slot ai ts timestamp) ps
          return . Archive $ lockstep ai timestamp $ before ++ after

-- TODO instead of passing the aiOffset, we could just pass the ArchiveInfo.
readPoint :: Whisper -> Int -> Int -> IO Point
readPoint Whisper{..} archiveOffset n =
  peekElemOff (castPtr whisperPtr `plusPtr` archiveOffset) n

readArchives :: Whisper -> Timestamp -> IO [Archive]
readArchives w timestamp = do
  Header{..} <- readHeader w
  mapM (flip (readArchive w) timestamp) [0..length hArchiveInfo - 1]

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

-- | Given an ArchiveInfo and a timestamp, return its slot in the archive.
slot ArchiveInfo{..} base timestamp = (((timestamp - base) `div` aiSecondsPerPoint) + 1) `mod` aiPoints

slot' ArchiveInfo{..} base timestamp = ((timestamp - base) `div` aiSecondsPerPoint) `mod` aiPoints

start ai timestamp = timestamp - (timestamp `mod` aiSecondsPerPoint ai) - aiRetention ai + aiSecondsPerPoint ai

slotTimestamp ai timestamp = timestamp - (timestamp `mod` aiSecondsPerPoint ai)

lockstep ai@ArchiveInfo{..} timestamp points = zipWith f [s, s + aiSecondsPerPoint ..] points
  where s = start ai timestamp
        f ts (Point ts' value) | ts == ts' = Point ts value
                               | otherwise = Point 0 0

-- | sizeOf a complete header, given the number of archives.
headerSize :: Int -> Int
headerSize n = sizeOf (undefined :: MetaData) + sizeOf (undefined :: ArchiveInfo) * n

-- | Given the precision and number of points of an archive (i.e. its
-- ArchiveInfo without its offset), and its predecessor archive, create a
-- complete ArchiveInfo (i.e. with its offset).
toAI :: ArchiveInfo -> (Int, Int) -> ArchiveInfo
toAI ArchiveInfo{..} (precision, points) =
  ArchiveInfo (aiOffset + sizeOf (undefined :: Point) * aiPoints) precision points

type Timestamp = Int

updateWhisper :: Whisper -> Header -> Int -> Double -> IO ()
updateWhisper w@Whisper{..} header timestamp value = do
  let archiveIndex = 0 -- TODO correctly select thearchive to update and propagate the value.
      ai = hArchiveInfo header !! archiveIndex
      offset = aiOffset ai
  Point ts _ <- readPoint w offset 0
  -- TODO then patter of reading the first point and decide if the archive has already some data is reused in readArchive.
  -- (B.t.w. I don't think it was necessary: only the modulo (without the base timestamp) was enough. Any gain
  -- exists only for empty or almost emtpy files.
  if ts == 0
    then writePoint w offset 0 (Point (slotTimestamp ai timestamp) value)
    else do
      let i = slot' (hArchiveInfo header !! archiveIndex) ts timestamp
      writePoint w offset i (Point (slotTimestamp ai timestamp )value)

instance Storable MetaData where
  sizeOf _ =
    sizeOf (undefined :: Word32)
    + sizeOf (undefined :: Word32)
    + sizeOf (undefined :: CFloat)
    + sizeOf (undefined :: Word32)

  alignment _ = alignment (undefined :: Word32)

  peek ptr = MetaData
    <$> peek (castPtr ptr)
    <*> peekWord32beAsNum (castPtr ptr `plusPtr` 4)
    <*> peekWord32beAsFloat (castPtr ptr `plusPtr` 8)
    <*> peekWord32beAsNum (castPtr ptr `plusPtr` 12)

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

  peek ptr = ArchiveInfo
    <$> peekWord32beAsNum (castPtr ptr)
    <*> peekWord32beAsNum (castPtr ptr `plusPtr` 4)
    <*> peekWord32beAsNum (castPtr ptr `plusPtr` 8)

  poke ptr ArchiveInfo{..} = do
    pokeBE (castPtr ptr) (fromIntegral aiOffset :: Word32)
    pokeBE (castPtr ptr `plusPtr` 4) (fromIntegral aiSecondsPerPoint :: Word32)
    pokeBE (castPtr ptr `plusPtr` 8) (fromIntegral aiPoints :: Word32)

instance Storable AggregationType where
  sizeOf _ = sizeOf (undefined :: Word32)

  alignment _ = alignment (undefined :: Word32)

  peek ptr = agg <$> peekBE (castPtr ptr :: Ptr Word32)
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

  peek ptr = Point
    <$> peekWord32beAsNum (castPtr ptr)
    <*> peekWord64beAsDouble (castPtr ptr `plusPtr` 4)

  poke ptr (Point a b) = do
    pokeBE (castPtr ptr) (fromIntegral a :: Word32)
    pokeBE (castPtr ptr `plusPtr` 4) (coerce b)
    where
    coerce :: Double -> Word64
    coerce x = unsafePerformIO $ with x $ \p ->
      peek (castPtr p) :: IO Word64

peekWord32beAsNum :: Num a => Ptr Word32 -> IO a
peekWord32beAsNum ptr = fromIntegral <$> peekBE ptr

peekWord32beAsFloat :: Ptr Word32 -> IO Float
peekWord32beAsFloat ptr = coerce <$> peekBE ptr
  where
  coerce :: Word32 -> Float
  coerce x = unsafePerformIO $ with x $ \p ->
    peek (castPtr p) :: IO Float

peekWord64beAsDouble :: Ptr Word64 -> IO Double
peekWord64beAsDouble ptr = coerce <$> peekBE ptr
  where
  coerce :: Word64 -> Double
  coerce x = unsafePerformIO $ with x $ \p ->
    peek (castPtr p) :: IO Double
