{-# LANGUAGE OverloadedStrings #-}

module Base
    (writeTree
    , readObj
    , commit
    ) where

import           Const                 (objectsDir, repoDir)
import Control.Monad ( forM, unless, when, forM_ )
import           Data
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8  as Utf8
import           Data.Foldable         (foldl')
import           Data.List             (intercalate, sort)
import           Data.Maybe            (catMaybes, mapMaybe)
import           System.Directory      (createDirectoryIfMissing,
                                        doesDirectoryExist,
                                        getDirectoryContents, removePathForcibly)
import           System.Exit           (exitFailure)
import           System.FilePath       ((</>))
import           System.IO             (hPutStrLn, stderr)

data ParsedObj
    = ParsedBlob BS.ByteString 
    | ParsedTree [TreeItem]
    | ParsedCommit [CommitHeader] BS.ByteString

data TreeItem = MkTreeItem ObjType BS.ByteString BS.ByteString deriving(Eq, Ord)
data CommitHeader = TreeHeader BS.ByteString | ParentHeader BS.ByteString

toParsedObj :: Obj -> ParsedObj
toParsedObj (MkObj Blob bs) = ParsedBlob bs
toParsedObj (MkObj Tree bs) = ParsedTree $ mapMaybe toTreeItem (Char8.lines bs)
toParsedObj (MkObj Commit bs) = _

fromParsedObj :: ParsedObj -> Obj
fromParsedObj (ParsedBlob bs) = MkObj Blob bs
fromParsedObj (ParsedTree items) = MkObj Tree $ (BS.intercalate "\n" . map fromTreeItem) items
fromParsedObj (ParsedCommit headers msg) = MkObj Commit $ _

fromTreeItem :: TreeItem -> BS.ByteString
fromTreeItem (MkTreeItem objType hash name) = getObjType objType <> " " <> hash <> " " <> name

toTreeItem :: BS.ByteString -> Maybe TreeItem
toTreeItem = parseItems . splitItems
    where
        splitItems = BS.split 32
        parseItems (objType:hash:name:xs) = Just $ MkTreeItem (toObjType objType) hash name
        parseItems _ = Nothing

writeTree :: FilePath -> IO BS.ByteString
writeTree dir = do
    when (dir == repoDir) (hPutStrLn stderr "Cannot write repository!" >> exitFailure)
    exists <- doesDirectoryExist dir
    unless exists (hPutStrLn stderr ("Directory " <> dir <> " not exists!") >> exitFailure)
    ds <- getDirectoryContents dir
    paths <- forM (filter validDir ds) $ \e -> do
        let path = dir </> e
        isDir <- doesDirectoryExist path
        if isDir then do
            hash <- writeTree path
            pure $ MkTreeItem Tree hash (Utf8.fromString e)
        else do
            content <- BS.readFile path
            hash <- hashObject (MkObj Blob content)
            pure $ MkTreeItem Blob hash (Utf8.fromString e)
    hashObject (fromParsedObj . ParsedTree $ sort paths)

listFiles :: FilePath -> IO [FilePath]
listFiles root = do
  ds <- getDirectoryContents root
  paths <- forM (filter validDir ds) $ \e -> do
    let path = root </> e
    isDir <- doesDirectoryExist path
    if isDir then listFiles path
    else return [path]
  return (concat paths)

validDir = (`notElem` [".","..",repoDir])

readObj :: String -> IO ()
readObj hash = emptyCurrentDir >> readObjWithBase "." hash
    where
        readObjWithBase :: FilePath -> String -> IO ()
        readObjWithBase root hash = do
            obj <- getObject hash
            maybe (hPutStrLn stderr ("Object " <> hash <> " not exists!")) (readObj' root) (toParsedObj <$> obj)

        readObj' :: FilePath -> ParsedObj -> IO ()
        readObj' path (ParsedCommit _ _) = mempty
        readObj' path (ParsedBlob content) = BS.writeFile path content

        readObj' path (ParsedTree items) = do
            createDirectoryIfMissing False path
            let actions = map (readItem path) items
            foldl' (>>) mempty actions
        readItem :: FilePath -> TreeItem -> IO ()
        readItem root (MkTreeItem Commit hash name) = mempty
        readItem root (MkTreeItem Tree hash name) = readObjWithBase (root </> Utf8.toString name) (Utf8.toString hash)
        readItem root (MkTreeItem Blob hash name) = do
            obj <- getObject (Utf8.toString hash)
            maybe (hPutStrLn stderr ("Object " <> Utf8.toString hash <> " not exists!")) (readObj' (root </> Utf8.toString name)) (toParsedObj <$> obj)

emptyCurrentDir :: IO ()
emptyCurrentDir = emptyDir "."
    where
        emptyDir :: FilePath -> IO ()
        emptyDir root = do
            ds <- getDirectoryContents root
            forM_ (filter validDir ds) $ \file -> do
                removePathForcibly file

commit :: String -> IO ()
commit msg = do
    hash <- writeTree "."
    headM <- getHEAD
    let header = [(getObjType Tree, hash)] <> maybe mempty (\head -> [("parent", head)]) headM
    let commitContent = buildCommit header (Utf8.fromString msg)
    commitHash <- hashObject (MkObj Commit commitContent)
    setHEAD commitHash
    where
        buildCommit :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString -> BS.ByteString
        buildCommit kvs msg = header <> "\n" <> msg
            where
                header = foldl' (\acc (k, v) -> acc <> k <> " " <> v <> "\n") "" kvs

-- getCommit :: String -> IO 