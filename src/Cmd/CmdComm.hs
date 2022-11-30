module Cmd.CmdComm where

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.IO (hPutStrLn, stderr, stdout)
import System.Exit (exitFailure)
import Const (repoDir)

preCheck :: IO a -> IO a
preCheck action = do
    repoExists <- doesDirectoryExist repoDir
    if repoExists then do 
        action
    else do
        hPutStrLn stderr "Repository not initialized."
        exitFailure