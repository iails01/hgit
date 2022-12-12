{-# LANGUAGE OverloadedStrings #-}

module Data
    ( hashObject
    , getObject
    , setHEAD
    , getHEAD
    , setRef
    , getRef
    , ObjType(..)
    , getObjType
    , toObjType
    , Obj(..)
    , Ref(..)
    , headRef
    , mkTagsRef
    , mkHeadsRef
) where

import           Const
import Control.Monad ( forM, when, MonadPlus(mzero) )
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
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Applicative ((<|>))
import System.FilePath.Posix (takeDirectory)

data ObjType = Blob | Tree | Commit deriving(Eq, Ord, Show)

data Obj = MkObj ObjType ByteString

newtype Ref = MkRef FilePath

data RefObj 
    = MkSymbolic Ref
    | MkDirect ByteString

headRef = MkRef "HEAD"
mkTagsRef tagName = MkRef ("refs" </> "tags" </> tagName)
mkHeadsRef tagName = MkRef ("refs" </> "heads" </> tagName)

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

getRef :: Ref -> MaybeT IO RefObj
getRef (MkRef path)
    =   getRef'' (MkRef $ "refs" </> "tags" </> path)
    <|> getRef'' (MkRef $ "refs" </> "heads" </> path)
    <|> getRef'' (MkRef $ "refs" </> path)
    <|> getRef'' (MkRef $ path)

    where
        getRef'' :: Ref -> MaybeT IO RefObj
        getRef'' ref = do
            val <- getRef' ref
            let (t, v) = breakSubstring ": " val
            if t == "ref" then do
                refObj <- getRef'' (MkRef . Utf8.toString . BS.drop 2 $ v)
                pure refObj {symbolic = True}
            else pure MkRefObj {ref = ref, symbolic = False, hash = val}
        getRef' :: Ref -> MaybeT IO ByteString
        getRef' (MkRef path) = do
            exists <- lift $ doesFileExist (repoDir </> path)
            if exists then do
                ei <- lift $ try (BS.readFile path) :: MaybeT IO (Either SomeException ByteString)
                case ei of
                    Left e -> mzero
                    Right v -> pure v
            else mzero

setRef :: Ref -> RefObj -> IO ()
setRef (MkRef path) refObj = do
    let file = repoDir </> path
    let dir = takeDirectory file
    createDirectoryIfMissing True dir
    BS.writeFile file (toContent refObj)
    where
        toContent :: RefObj -> ByteString
        toContent MkRefObj{ref = MkRef refname, symbolic = isSymbolic, hash = hash} = 
            if isSymbolic then "ref: " <> Utf8.fromString refname else hash

setHEAD :: ByteString -> IO ()
setHEAD = setRef headRef

getHEAD :: MaybeT IO ByteString
getHEAD = getRef headRef
