module Cmd.HashObject(Opt(..), hashObject) where

import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString.UTF8(fromString, toString, foldl)
import Const
import System.Directory (createDirectoryIfMissing)

newtype Opt = Opt String

hashObject :: Opt -> IO ()
hashObject (Opt file) = do
  content <- readFile file
  let hash = Data.ByteString.UTF8.foldl (flip (:)) "" $ SHA1.hash $ fromString content
  createDirectoryIfMissing False objectsDir
  writeFile (objectsDir <> "/" <> hash) content
  putStrLn ("saved: " <> hash)
    
