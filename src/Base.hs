{-# LANGUAGE OverloadedStrings #-}

module Base(writeTree, readObj) where

import Const ( repoDir, objectsDir )
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Directory (doesDirectoryExist, getDirectoryContents, createDirectoryIfMissing)
import Control.Monad (when, forM)
import System.FilePath ((</>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8 as Utf8
import Data.List (sort, intercalate)
import Data
    ( ObjType(..),
      hashObject,
      getObjType,
      getObject,
      Obj(MkObj),
      toObjType )
import Data.Maybe (catMaybes, mapMaybe)
import Data.Foldable (foldl')

data TreeItem = MkTreeItem ObjType BS.ByteString BS.ByteString deriving(Eq, Ord)

toTreeItemLine :: TreeItem -> BS.ByteString
toTreeItemLine (MkTreeItem objType hash name) = getObjType objType <> " " <> hash <> " " <> name

toTreeItemLines :: [TreeItem] -> BS.ByteString
toTreeItemLines = BS.intercalate "\n" . map toTreeItemLine

fromTreeItemLines :: BS.ByteString -> [TreeItem]
fromTreeItemLines bs = mapMaybe fromTreeItemLine (Char8.lines bs)

fromTreeItemLine :: BS.ByteString -> Maybe TreeItem
fromTreeItemLine = parseItems . splitItems
    where
        splitItems = BS.split 32
        parseItems (objType:hash:name:xs) = Just $ MkTreeItem (toObjType objType) hash name
        parseItems _ = Nothing

writeTree :: FilePath -> IO BS.ByteString
writeTree dir = do
    when (dir == repoDir) (hPutStrLn stderr "Cannot write repository!" >> exitFailure)
    exists <- doesDirectoryExist dir
    when (not exists) (hPutStrLn stderr ("Directory " <> dir <> " not exists!") >> exitFailure)
    ds <- getDirectoryContents dir
    paths <- forM (filter (`notElem` [".","..",repoDir]) ds) $ \e -> do
        let path = dir </> e
        isDir <- doesDirectoryExist path
        if isDir then do
            hash <- writeTree path
            pure $ MkTreeItem Tree hash (Utf8.fromString e)
        else do
            content <- BS.readFile path
            hash <- hashObject Blob content
            pure $ MkTreeItem Blob hash (Utf8.fromString e)
    hashObject Tree (toTreeItemLines $ sort paths)

listFiles :: FilePath -> IO [FilePath]
listFiles root = do
  ds <- getDirectoryContents root
  paths <- forM (filter (`notElem` [".","..",repoDir]) ds) $ \e -> do
    let path = root </> e
    isDir <- doesDirectoryExist path
    if isDir then listFiles path
    else return [path]
  return (concat paths)

readObj :: String -> IO ()
readObj = readObjWithBase "."
    where
        readObjWithBase :: FilePath -> String -> IO ()
        readObjWithBase root hash = do
            obj <- getObject hash
            maybe (hPutStrLn stderr ("Object " <> hash <> " not exists!")) (readObj' root) obj

        readObj' :: FilePath -> Obj -> IO ()
        readObj' path (MkObj Blob content) = BS.writeFile path content

        readObj' path (MkObj Tree lines) = do
            createDirectoryIfMissing False path
            let items = fromTreeItemLines lines
            let actions = map (readItem path) items
            foldl' (>>) mempty actions
        readItem :: FilePath -> TreeItem -> IO ()
        readItem root (MkTreeItem Tree hash name) = readObjWithBase (root </> Utf8.toString name) (Utf8.toString hash)
        readItem root (MkTreeItem Blob hash name) = do
            obj <- getObject (Utf8.toString hash)
            maybe (hPutStrLn stderr ("Object " <> Utf8.toString hash <> " not exists!")) (readObj' (root </> Utf8.toString name)) obj
