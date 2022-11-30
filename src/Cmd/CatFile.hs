{-# LANGUAGE OverloadedStrings #-}

module Cmd.CatFile where

import Cmd.CmdComm (preCheck)
import qualified Data.ByteString.Char8 as Char8
import Data (getObject)

data Opt = Opt String

catFile :: Opt -> IO ()
catFile (Opt hash) = preCheck $ do
    content <- getObject hash
    maybe (pure ()) Char8.putStrLn content
    