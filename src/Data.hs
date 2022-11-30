{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data(hashObject, getObject, ObjType(..), getObjType) where
import Text.Printf (printf)
import Crypto.Hash.SHA1 (hashlazy, hash)
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist, getDirectoryContents)
import Const (objectsDir, repoDir)
import Data.ByteString (ByteString, unpack, breakSubstring)
import Data.ByteString as BS
import Control.Monad (when, forM)
import System.FilePath ((</>))
import System.Exit (exitFailure)
import System.IO ( stderr, hPutStrLn )
import Data.String(fromString)

data ObjType = Blob | Tree deriving(Eq, Ord)

hashObject :: ObjType -> ByteString -> IO ByteString 
hashObject objType content = do
    let hash = toHexHash content
    createDirectoryIfMissing False objectsDir
    BS.writeFile (objectsDir </> hash) (getObjType objType <> "\0" <> content)
    pure $ fromString hash

getObjType :: ObjType -> ByteString
getObjType Blob = "blob"
getObjType Tree = "tree"

toHexHash :: ByteString -> String
toHexHash = toHex . hash
  where
    toHex :: ByteString -> String
    toHex bytes = unpack bytes >>= printf "%02x"

test :: IO ()
test = do
    print $ toHexHash "this is cool" == "60f51187e76a9de0ff3df31f051bde04da2da891"

getObject :: String -> IO (Maybe ByteString)
getObject hash = do
    let file = objectsDir </> hash
    exists <- doesFileExist file
    if exists then do
        bs <- BS.readFile file
        let (fileType, content) = breakSubstring "\0" bs
        pure $ Just content
    else pure Nothing