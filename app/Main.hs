module Main where

import Opt(CmdOpts(..), execParser)
import Cmd.Init(initRepo)
import Cmd.HashObject(hashObject)
import Cmd.CatFile (catFile)


main :: IO ()
main = do
    opts <- execParser
    handleCmd opts

handleCmd :: CmdOpts -> IO ()
handleCmd Init = initRepo
handleCmd (HashObject opt) = hashObject opt
handleCmd (CatFile opt) = catFile opt