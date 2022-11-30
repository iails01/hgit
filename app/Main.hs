module Main where

import Opt(CmdOpts(..), execParser)
import Cmd(initRepo, hashObject, catFile, writeTree)

main :: IO ()
main = do
    opts <- execParser
    handleCmd opts

handleCmd :: CmdOpts -> IO ()
handleCmd Init = initRepo
handleCmd (HashObject opt) = hashObject opt
handleCmd (CatFile opt) = catFile opt
handleCmd (WriteTree opt) = writeTree opt