{-# LANGUAGE OverloadedStrings #-}

module Cmd.HashObject(Opt(..), hashObject) where

import Const
import Cmd.CmdComm (preCheck)
import qualified Data 

newtype Opt = Opt String

hashObject :: Opt -> IO ()
hashObject (Opt file) = preCheck $ do
    hash <- Data.hashObject file
    putStrLn ("saved: " <> hash)

