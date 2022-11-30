{-# LANGUAGE OverloadedStrings #-}

module Cmd.HashObject(Opt(..), hashObject) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Const
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import Text.Printf (printf)
import Crypto.Hash.SHA1 (hashlazy)
import Control.Monad (when)
import Cmd.CmdComm (preCheck)

newtype Opt = Opt String

hashObject :: Opt -> IO ()
hashObject (Opt file) = preCheck $ do
    content <- Lazy.readFile file
    let hash = toHexHash content
    createDirectoryIfMissing False objectsDir
    Lazy.writeFile (objectsDir <> "/" <> hash) content
    putStrLn ("saved: " <> hash)

toHexHash :: Lazy.ByteString -> String
toHexHash = toHex . hashlazy
  where
    toHex :: Strict.ByteString -> String
    toHex bytes = Strict.unpack bytes >>= printf "%02x"

test :: IO ()
test = do
    print $ toHexHash "this is cool" == "60f51187e76a9de0ff3df31f051bde04da2da891"
