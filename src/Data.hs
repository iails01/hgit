{-# LANGUAGE OverloadedStrings #-}

module Data(hashObject, getObject, ObjType(..), getObjType) where
import Text.Printf (printf)
import Crypto.Hash.SHA1 (hashlazy, hash)
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist, getDirectoryContents)
import Const
import Data.ByteString (ByteString, unpack, breakSubstring)
import Data.ByteString as BS
import Control.Monad (when, forM)
import System.FilePath ((</>))
import System.Exit (exitFailure)
import System.IO ( stderr, hPutStrLn )
import Data.String(fromString)
import Util

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

getObject :: String -> IO (Maybe ByteString)
getObject hash = do
    let file = objectsDir </> hash
    exists <- doesFileExist file
    if exists then do
        bs <- BS.readFile file
        let (fileType, content) = breakSubstring "\0" bs
        pure $ Just content
    else pure Nothing