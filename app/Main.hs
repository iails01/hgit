module Main where

import Opt(CmdOpts(..), execParser)

main :: IO ()
main = do
    opts <- execParser
    handleCmd opts

handleCmd :: CmdOpts -> IO ()
handleCmd Init = putStrLn "Hello, World!"
