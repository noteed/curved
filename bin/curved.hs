{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Data.Char (toLower)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Console.CmdArgs.Implicit

import Curved.Carbon
import Curved.Httpd
import Data.Whisper

main :: IO ()
main = (runCmd =<<) . cmdArgs $
  modes
    [ cmdInfo
    , cmdHttpd
    , cmdPush
    , cmdCreate
    ]
  &= summary versionString
  &= program "curved"

-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "Curved. Copyright (c) 2012 Vo Minh Thu."
  -- TODO add the version.

-- | Data type representing the different command-line subcommands.
data Cmd =
    Info { cmdFilename :: FilePath }
    -- ^ Display some Whisper file information.
  | Httpd
    -- ^ Run the curved web server.
  | Push { cmdFilename :: FilePath, cmdValue :: Double }
    -- ^ Add a new timestamped value to a Whisper file.
  | Create { cmdFilename :: FilePath, cmdPrecision :: Int, cmdSize :: Int }
    -- ^ Create a new Whisper file.
  deriving (Data, Typeable)

-- | Create a 'Info' command.
cmdInfo :: Cmd
cmdInfo = Info
  { cmdFilename = def
    &= argPos 0
    &= typ "FILE"
  } &= help "Display some Whisper file information."
    &= explicit
    &= name "info"

-- | Create a 'Httpd' command.
cmdHttpd :: Cmd
cmdHttpd = Httpd
    &= help "Run the Curved web server."
    &= explicit
    &= name "httpd"

-- | Create a 'Push' command.
cmdPush :: Cmd
cmdPush = Push
  { cmdFilename = def
    &= argPos 0
    &= typ "FILE"
  , cmdValue = def
    &= argPos 1
    &= typ "DOUBLE"
  } &= help "Add a new timestamped value to a Whisper file."
    &= explicit
    &= name "push"

-- | Create a 'Create' command.
cmdCreate :: Cmd
cmdCreate = Create
  { cmdFilename = def
    &= argPos 0
    &= typ "FILE"
  , cmdPrecision = def
    &= explicit
    &= name "precision"
    &= help "Timeframe (in seconds) covered by each point."
  , cmdSize = def
    &= explicit
    &= name "size"
    &= help "Number of points contained in the archive."
  } &= help "Create a new Whisper file with a single archive."
    &= explicit
    &= name "create"

-- | Run a sub-command.
runCmd :: Cmd -> IO ()
runCmd Info{..} = do
  w <- openWhisper cmdFilename
  now <- (floor . toRational) <$> getPOSIXTime
  archives <- readArchives w now
  let Header{..} = whisperHeader w
      MetaData{..} = hMetaData
  closeWhisper w

  putStrLn $ cmdFilename ++ ": period=" ++ show mdMaxRetention
    ++ ", aggregation=" ++ map toLower (show mdAggregationType)
    ++ ", propagation=" ++ show mdXFilesFactor
    ++ ", archives=" ++ show mdArchiveCount
  let f ai@ArchiveInfo{..} = "  period=" ++ show (aiRetention ai)
        ++ ", points=" ++ show aiPoints
        ++ ", seconds=" ++ show aiSecondsPerPoint
        ++ ", size=" ++ show (aiSize ai)
        ++ ", offset=" ++ show aiOffset
  mapM_ (putStrLn . f) hArchiveInfo
  mapM_ (\(Archive points) -> mapM_ print points) archives

runCmd Httpd{..} = do
  forkIO $ receivePoints 2006
  forkIO $ receiveQueries 7002
  httpd 8081 "localhost"

runCmd Push{..} = do
  now <- (floor . toRational) <$> getPOSIXTime
  updateWhisperFile cmdFilename now cmdValue

runCmd Create{..} =
  createWhisper cmdFilename [(cmdPrecision, cmdSize)] 0.5 Average
