module Main where

import Opt(CmdOpts(..), execParser)
import Cmd.Init(initRepo)
import Cmd.HashObject(hashObject)


main :: IO ()
main = do
    opts <- execParser
    handleCmd opts

handleCmd :: CmdOpts -> IO ()
handleCmd Init = initRepo

handleCmd (HashObject opt) = hashObject opt