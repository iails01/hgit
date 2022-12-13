{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Data
    ( hashObject
    , getObject
    , setHEAD
    , getHEAD
    , setRef
    , getRef
    , getDeRef
    , RefObjKind(..)
    , RefObj(..)
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
import qualified          Data.ByteString      as BS
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

data Obj = MkObj ObjType BS.ByteString

newtype Ref = MkRef FilePath

data RefObjKind = RefKind | DerefKind

data RefObj (a :: RefObjKind) where
    MkDirect :: BS.ByteString -> RefObj RefKind
    MkSymbolic :: Ref -> RefObj RefKind
    MkDereference :: Ref -> BS.ByteString -> RefObj DerefKind

headRef = MkRef "HEAD"
mkTagsRef tagName = MkRef ("refs" </> "tags" </> tagName)
mkHeadsRef tagName = MkRef ("refs" </> "heads" </> tagName)

hashObject :: Obj -> IO BS.ByteString
hashObject (MkObj objType fileContent) = do
    let content = getObjType objType <> "\0" <> fileContent
    let hash = toHexHash content
    createDirectoryIfMissing False objectsDir
    BS.writeFile (objectsDir </> hash) content
    pure $ Utf8.fromString hash

getObjType :: ObjType -> BS.ByteString
getObjType Blob = "blob"
getObjType Tree = "tree"
getObjType Commit = "commit"

toObjType :: BS.ByteString -> ObjType
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
        let (fileType, content) = BS.breakSubstring "\0" bs
        pure . Just $ MkObj (toObjType fileType) (BS.drop 1 content)
    else pure Nothing

getRefVal :: Ref -> MaybeT IO BS.ByteString
getRefVal (MkRef path) = do
    let file = repoDir </> path
    exists <- lift $ doesFileExist file
    if exists then do
        ei <- lift $ try (BS.readFile file) :: MaybeT IO (Either SomeException BS.ByteString)
        case ei of
            Left e -> mzero
            Right v -> pure v
    else mzero

allRefs :: Ref -> [Ref]
allRefs (MkRef path) = fmap (\p -> MkRef $ p </> path) ["refs" </> "tags", "refs" </> "heads", "refs", ""]

getDeRef :: Ref -> MaybeT IO (RefObj DerefKind)
getDeRef ref = Prelude.foldl (\ acc r -> acc <|> getDeRef' r) mzero (allRefs ref)
    where
        getDeRef' ref = do
            val <- getRefVal ref
            let (t, v) = BS.breakSubstring ": " val
            if t == "ref" then do
                refObj <- getDeRef' (MkRef . Utf8.toString . BS.drop 2 $ v)
                pure $ case refObj of
                    MkDereference _ hash -> MkDereference ref hash
            else pure $ MkDereference ref val

getRef ::  Ref -> MaybeT IO (RefObj RefKind)
getRef ref = Prelude.foldl (\ acc r -> acc <|> getRef' r) mzero (allRefs ref)
    where
        getRef' :: Ref -> MaybeT IO (RefObj RefKind)
        getRef' ref = do
            val <- getRefVal ref
            let (t, v) = BS.breakSubstring ": " val
            if t == "ref" then pure $ MkSymbolic (MkRef . Utf8.toString . BS.drop 2 $ v)
            else pure $ MkDirect val

setRef :: Ref -> RefObj RefKind -> IO ()
setRef (MkRef path) refObj = do
    let file = repoDir </> path
    let dir = takeDirectory file
    createDirectoryIfMissing True dir
    BS.writeFile file (toContent refObj)
    where
        toContent :: RefObj RefKind -> BS.ByteString
        toContent (MkDirect hash) = hash
        toContent (MkSymbolic (MkRef ref)) = "ref: " <> Utf8.fromString ref

setHEAD :: RefObj RefKind -> IO ()
setHEAD = setRef headRef

getHEAD :: MaybeT IO BS.ByteString
getHEAD = do
    (MkDereference _ hash) <- getDeRef headRef
    pure hash
