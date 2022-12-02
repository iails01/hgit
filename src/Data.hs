{-# LANGUAGE OverloadedStrings #-}

module Data(hashObject, getObject, setHEAD, getHEAD, ObjType(..), getObjType, toObjType, Obj(..)) where
import           Const
import           Control.Monad        (forM, when)
import           Crypto.Hash.SHA1     (hash, hashlazy)
import           Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as Utf8
import           System.Directory     (createDirectoryIfMissing,
                                       doesDirectoryExist, doesFileExist,
                                       getDirectoryContents)
import           System.Exit          (exitFailure)
import           System.FilePath      ((</>))
import           System.IO            (hPutStrLn, stderr)
import           Text.Printf          (printf)
import           Util
import Control.Exception (try, SomeException (SomeException))

data ObjType = Blob | Tree | Commit deriving(Eq, Ord)

data Obj = MkObj ObjType ByteString

hashObject :: Obj -> IO ByteString
hashObject (MkObj objType fileContent) = do
    let content = getObjType objType <> "\0" <> fileContent
    let hash = toHexHash content
    createDirectoryIfMissing False objectsDir
    BS.writeFile (objectsDir </> hash) content
    pure $ Utf8.fromString hash

getObjType :: ObjType -> ByteString
getObjType Blob = "blob"
getObjType Tree = "tree"
getObjType Commit = "commit"

toObjType :: ByteString -> ObjType
toObjType "blob" = Blob
toObjType "tree" = Tree
toObjType "commit" = Commit
toObjType _      = Blob

getObject :: String -> IO (Maybe Obj)
getObject hash = do
    let file = objectsDir </> hash
    exists <- doesFileExist file
    if exists then do
        bs <- BS.readFile file
        let (fileType, content) = breakSubstring "\0" bs
        pure . Just $ MkObj (toObjType fileType) (BS.drop 1 content)
    else pure Nothing

setHEAD :: ByteString -> IO ()
setHEAD = BS.writeFile headFile

getHEAD :: IO (Maybe ByteString) 
getHEAD = do
    ei <- try (BS.readFile headFile) :: IO (Either SomeException ByteString)
    case ei of
        Left e -> pure Nothing
        Right v -> pure $ Just v