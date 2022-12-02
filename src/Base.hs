{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Base
    (writeTree
    , readObj
    , commit
    , log
    ) where

import           Const
import Control.Monad ( forM, unless, when, forM_ )
import           Data
import qualified Data.ByteString       as BS
import qualified Data.ByteString.UTF8  as Utf8
import           Data.Foldable         (foldl')
import           Data.List             (intercalate, sort, partition)
import           Data.Maybe            (catMaybes, mapMaybe)
import           System.Directory      (createDirectoryIfMissing,
                                        doesDirectoryExist,
                                        getDirectoryContents, removePathForcibly)
import           System.Exit           (exitFailure)
import           System.FilePath       ((</>))
import           System.IO             (hPutStrLn, stderr)
import qualified Data.ByteString.Char8 as Char8
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Util
import Prelude hiding (log)

data ParsedObj
    = ParsedBlob BS.ByteString
    | ParsedTree [TreeItem]
    | ParsedCommit [CommitHeader] BS.ByteString

data TreeItem = MkTreeItem ObjType BS.ByteString BS.ByteString deriving(Eq, Ord)
data CommitHeader = TreeHeader BS.ByteString | ParentHeader BS.ByteString

toParsedObj :: Obj -> ParsedObj
toParsedObj (MkObj Blob bs) = ParsedBlob bs
toParsedObj (MkObj Tree bs) = ParsedTree $ mapMaybe toTreeItem (Utf8.lines bs)
toParsedObj (MkObj Commit bs) = ParsedCommit (mapMaybe toCommitHeader headerLines) (BS.concat msgLines)
    where
        (headerLines, msgLines) = partition ( == "") (Utf8.lines bs)

fromParsedObj :: ParsedObj -> Obj
fromParsedObj (ParsedBlob bs) = MkObj Blob bs
fromParsedObj (ParsedTree items) = MkObj Tree $ (BS.intercalate "\n" . map fromTreeItem) items
fromParsedObj (ParsedCommit headers msg) = MkObj Commit (Char8.unlines (map fromCommitHeader headers) <> "\n" <> msg)

fromTreeItem :: TreeItem -> BS.ByteString
fromTreeItem (MkTreeItem objType hash name) = getObjType objType <> " " <> hash <> " " <> name

toTreeItem :: BS.ByteString -> Maybe TreeItem
toTreeItem = parseItems . splitItems
    where
        splitItems = BS.split spaceChar
        parseItems (objType:hash:name:xs) = Just $ MkTreeItem (toObjType objType) hash name
        parseItems _ = Nothing

fromCommitHeader :: CommitHeader -> BS.ByteString
fromCommitHeader (TreeHeader v) = "tree " <> v
fromCommitHeader (ParentHeader v) = "parent " <> v

toCommitHeader :: BS.ByteString -> Maybe CommitHeader
toCommitHeader = parseItems . splitItems
    where
        splitItems = BS.split spaceChar
        parseItems ("tree ":value:xs) = Just $ TreeHeader value
        parseItems ("parent ":value:xs) = Just $ ParentHeader value
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
    let headers = [TreeHeader hash] <> maybe mempty (\head -> [ParentHeader head]) headM
    let comm = ParsedCommit headers (Utf8.fromString msg)
    commitHash <- hashObject $ fromParsedObj comm
    setHEAD commitHash

getCommit :: String -> IO (Maybe ParsedObj)
getCommit hash = do
    objM <- getObject hash
    pure $ toParsedObj <$> objM

maybeM :: Maybe a -> (a -> IO ()) -> IO ()
maybeM (Just a) f = f a
maybeM Nothing _ = pure ()

parentHash :: ParsedObj -> Maybe (BS.ByteString, BS.ByteString)
parentHash (ParsedBlob _) = Nothing 
parentHash (ParsedTree _) = Nothing
parentHash (ParsedCommit headers msg) = safeHead (mapMaybe mapper headers) >>= \parent -> pure (parent, msg)
    where
        mapper (ParentHeader parent) = Just parent
        mapper _ = Nothing 

log :: IO ()
log = do
    hashM <- getHEAD
    maybeM hashM (\hash -> do
        commM <- getCommit (Utf8.toString hash)
        maybeM commM (\comm -> do 
                let phashM = parentHash comm
                maybeM phashM (\(phash, msg) -> (putStrLn . Utf8.toString) (phash <> "\n" <> msg))
            )
        )
