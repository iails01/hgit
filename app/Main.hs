module Main where

import Opt
import Cmd

main :: IO ()
main = do
    opts <- execParser
    handleCmd opts

handleCmd :: CmdOpts -> IO ()
handleCmd Init = initRepo
handleCmd (HashObject opt) = hashObject opt
handleCmd (CatFile opt) = catFile opt
handleCmd (WriteTree opt) = writeTree opt
handleCmd (ReadTree opt) = readTree opt