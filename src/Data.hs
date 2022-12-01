{-# LANGUAGE OverloadedStrings #-}

module Data(hashObject, getObject, ObjType(..), getObjType, toObjType, Obj(..)) where
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

data Obj = MkObj ObjType ByteString

hashObject :: ObjType -> ByteString -> IO ByteString
hashObject objType fileContent = do
    let content = getObjType objType <> "\0" <> fileContent
    let hash = toHexHash content
    createDirectoryIfMissing False objectsDir
    BS.writeFile (objectsDir </> hash) content
    pure $ fromString hash

getObjType :: ObjType -> ByteString
getObjType Blob = "blob"
getObjType Tree = "tree"

toObjType :: ByteString -> ObjType
toObjType "blob" = Blob
toObjType "tree" = Tree
toObjType _ = Blob

getObject :: String -> IO (Maybe Obj)
getObject hash = do
    let file = objectsDir </> hash
    exists <- doesFileExist file
    if exists then do
        bs <- BS.readFile file
        let (fileType, content) = breakSubstring "\0" bs
        pure . Just $ MkObj (toObjType fileType) (BS.drop 1 content)
    else pure Nothing