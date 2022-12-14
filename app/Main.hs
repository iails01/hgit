module Main where

import Opt
import Cmd
import Prelude hiding(log)

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
handleCmd (Commit opt) = commit opt
handleCmd (Log opt) = log opt
handleCmd (Checkout opt) = checkout opt
handleCmd (Tag opt) = tag opt
handleCmd (Klog opt) = klog opt
handleCmd (Branch opt) = branch opt
handleCmd (Status opt) = status opt
handleCmd (Reset opt) = reset opt