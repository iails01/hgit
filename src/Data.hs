{-# LANGUAGE OverloadedStrings #-}

module Data(hashObject, getObject) where
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Text.Printf (printf)
import Crypto.Hash.SHA1 (hashlazy)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Const (objectsDir)
import Data.ByteString (ByteString)
import Control.Monad (when)


hashObject :: FilePath -> IO String
hashObject file = do
    let fileType = getFileType file
    content <- Lazy.readFile file
    let hash = toHexHash content
    createDirectoryIfMissing False objectsDir
    Lazy.writeFile (objectsDir <> "/" <> hash) (fileType <> "\0" <> content)
    pure hash


getFileType :: FilePath -> Lazy.ByteString
getFileType _ = "blob"

toHexHash :: Lazy.ByteString -> String
toHexHash = toHex . hashlazy
  where
    toHex :: Strict.ByteString -> String
    toHex bytes = Strict.unpack bytes >>= printf "%02x"

test :: IO ()
test = do
    print $ toHexHash "this is cool" == "60f51187e76a9de0ff3df31f051bde04da2da891"

getObject :: String -> IO (Maybe ByteString)
getObject hash = do
    let file = objectsDir <> "/" <> hash
    exists <- doesFileExist file
    if exists then do
        bs <- Strict.readFile file
        let (fileType, content) = Strict.breakSubstring "\0" bs
        pure $ Just content
    else pure Nothing