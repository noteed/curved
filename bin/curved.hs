{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Char (toLower)
import System.Console.CmdArgs.Implicit

import Curved.Httpd
import Data.Whisper

main :: IO ()
main = (runCmd =<<) . cmdArgs $
  modes
    [ cmdInfo
    , cmdHttpd
    , cmdPush
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
  | Push
    -- ^ Add a new timestamped value to a Whisper file.
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
    &= help "Add a new timestamped value to a Whisper file."
    &= explicit
    &= name "push"

-- | Run a sub-command.
runCmd :: Cmd -> IO ()
runCmd Info{..} = do
  wsp <- openWhisper cmdFilename
  Header{..} <- readHeader wsp
  let MetaData{..} = hMetaData
  closeWhisper wsp

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

runCmd Httpd{..} = httpd 8080 "localhost"

runCmd Push{..} = do
  putStrLn "`push` command not implemented." -- TODO
