{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import System.Console.CmdArgs.Implicit

import Data.Whisper

main :: IO ()
main = (runCmd =<<) . cmdArgs $
  modes
    [ cmdInfo
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
  header <- readHeader wsp
  print header
  archives <- readArchives wsp
  print archives
  closeWhisper wsp

runCmd Push{..} = do
  putStrLn "`push` command not implemented." -- TODO
