{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Base
    (writeTree
    , readObj
    , commit
    , log
    , checkout
    , tag
    , resolveOid
    , klog
    ) where

import           Const
import Control.Monad ( forM, unless, when, forM_, foldM )
import           Data
import qualified Data.ByteString       as BS
import qualified Data.ByteString.UTF8  as Utf8
import           Data.Foldable         (foldl')
import           Data.List             (intercalate, sort, partition)
import           Data.Maybe            (catMaybes, mapMaybe)
import           System.Directory      (createDirectoryIfMissing,
                                        doesDirectoryExist,
                                        getDirectoryContents, removePathForcibly, doesFileExist)
import           System.Exit           (exitFailure)
import           System.FilePath       ((</>), makeRelative)
import           System.IO             (hPutStrLn, stderr, hPutStr, hClose)
import qualified Data.ByteString.Char8 as Char8
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Util
import Prelude hiding (log)
import Control.Monad (MonadPlus(mzero))
import Control.Applicative ((<|>))
import System.Process
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Base (join)
import Control.Monad.Trans.State (StateT (runStateT), get)

data ParsedObj
    = ParsedBlob BS.ByteString BS.ByteString
    | ParsedTree BS.ByteString [TreeItem]
    | ParsedCommit BS.ByteString CommitHeaders BS.ByteString
    deriving(Show)

data TreeItem = MkTreeItem ObjType BS.ByteString BS.ByteString deriving(Eq, Ord, Show)
data CommitHeader = TreeHeader BS.ByteString | ParentHeader BS.ByteString deriving(Show)
data CommitHeaders = MkCommitHeaders {raw :: [CommitHeader], tree :: BS.ByteString, parent :: Maybe BS.ByteString} deriving(Show)

parsedObjHash :: ParsedObj -> BS.ByteString
parsedObjHash (ParsedBlob hash _) = hash
parsedObjHash (ParsedTree hash _) = hash
parsedObjHash (ParsedCommit hash _ _) = hash

toParsedObj :: BS.ByteString -> Obj -> ParsedObj
toParsedObj hash (MkObj Blob bs) = ParsedBlob hash bs
toParsedObj hash (MkObj Tree bs) = ParsedTree hash $ mapMaybe toTreeItem (Utf8.lines bs)
toParsedObj hash (MkObj Commit bs) = ParsedCommit hash (toHeaders (mapMaybe toCommitHeader headerLines)) (BS.concat msgLines)
    where
        (headerLines, msgLines) = break ( == "") (Utf8.lines bs)

fromParsedObj :: ParsedObj -> Obj
fromParsedObj (ParsedBlob _ bs) = MkObj Blob bs
fromParsedObj (ParsedTree _ items) = MkObj Tree $ (BS.intercalate "\n" . map fromTreeItem) items
fromParsedObj (ParsedCommit _ MkCommitHeaders{raw=headers} msg) = MkObj Commit (Char8.unlines (map fromCommitHeader headers) <> "\n" <> msg)

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
        parseItems ("tree":value:xs) = Just $ TreeHeader value
        parseItems ("parent":value:xs) = Just $ ParentHeader value
        parseItems _ = Nothing

toHeaders :: [CommitHeader] -> CommitHeaders
toHeaders hs = foldl' put MkCommitHeaders {raw = hs, tree = error "Commit no tree header!", parent = Nothing} hs
    where
        put headers (TreeHeader bs) = headers {tree = bs}
        put headers (ParentHeader bs) = headers {parent = Just bs}

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
    hashObject (fromParsedObj . ParsedTree "" $ sort paths)

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
readObj oid = do
    hashM <- runMaybeT $ resolveOid oid
    case hashM of
        Just hash -> emptyCurrentDir >> readObjWithBase "." (Utf8.toString hash)
        Nothing -> pure ()
    where
        readObjWithBase :: FilePath -> String -> IO ()
        readObjWithBase root hash = do
            obj <- getObject hash
            maybe (hPutStrLn stderr ("Object " <> hash <> " not exists!")) (readObj' root) (toParsedObj (Utf8.fromString hash) <$> obj)

        readObj' :: FilePath -> ParsedObj -> IO ()
        readObj' path ParsedCommit {} = mempty
        readObj' path (ParsedBlob _ content) = BS.writeFile path content

        readObj' path (ParsedTree _ items) = do
            createDirectoryIfMissing False path
            let actions = map (readItem path) items
            foldl' (>>) mempty actions
        readItem :: FilePath -> TreeItem -> IO ()
        readItem root (MkTreeItem Commit hash name) = mempty
        readItem root (MkTreeItem Tree hash name) = readObjWithBase (root </> Utf8.toString name) (Utf8.toString hash)
        readItem root (MkTreeItem Blob hash name) = do
            obj <- getObject (Utf8.toString hash)
            maybe (hPutStrLn stderr ("Object " <> Utf8.toString hash <> " not exists!")) (readObj' (root </> Utf8.toString name)) (toParsedObj hash <$> obj)

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
    headM <- runMaybeT getHEAD
    let headers = [TreeHeader hash] <> maybe mempty (\head -> [ParentHeader head]) headM
    let comm = ParsedCommit "" (toHeaders headers) (Utf8.fromString msg)
    commitHash <- hashObject $ fromParsedObj comm
    setHEAD commitHash
    putStrLn . Utf8.toString $ commitHash

getCommit :: String -> MaybeT IO ParsedObj
getCommit oid = do
    hash <- resolveOid oid
    objM <- lift $ getObject (Utf8.toString hash)
    let resultM = toParsedObj hash <$> objM
    MaybeT . pure $ resultM

log :: String -> IO ()
log oid = do
    runMaybeT $ do
        comm <- getCommit oid
        printLog comm
    pure ()

    where
        printLog :: ParsedObj -> MaybeT IO ()
        printLog (ParsedBlob hash _) = mzero
        printLog (ParsedTree hash _) = mzero
        printLog (ParsedCommit hash headers msg) = do
            lift $ putStrLn ("commit " <> Utf8.toString hash <> "\n\n\t" <> Utf8.toString msg <> "\n")
            printParent headers

        printParent :: CommitHeaders -> MaybeT IO ()
        printParent MkCommitHeaders{tree = t, parent = Nothing} = mzero
        printParent MkCommitHeaders{tree = t, parent = Just p} = do
            let pStr = Utf8.toString p
            comm <- getCommit pStr
            printLog comm

checkout :: String -> IO ()
checkout oid = do
    runMaybeT $ do
        comm <- getCommit oid
        hash <- readCommit comm
        lift $ setHEAD hash
    pure ()
    where
        readCommit :: ParsedObj -> MaybeT IO BS.ByteString
        readCommit (ParsedCommit hash MkCommitHeaders{tree=t} _) = do
            lift $ readObj . Utf8.toString $ t
            pure hash
        readCommit _ = mzero

resolveOid :: String -> MaybeT IO BS.ByteString
resolveOid oid = getRef (MkRef oid) <|> objHash oid <|> (lift (hPutStrLn stderr ("Cannot resolve this oid: " <> oid)) >> mzero)
    where
        objHash :: String -> MaybeT IO BS.ByteString
        objHash hash = do
            exists <- lift $ doesFileExist (objectsDir </> hash)
            if exists then pure (Utf8.fromString hash)
            else mzero

tag :: String -> String -> IO ()
tag tagName oid = do
    hashM <- runMaybeT $ resolveOid oid
    maybe (hPutStrLn stderr (oid <> " not exists!") >> exitFailure) (setRef (mkTagsRef tagName)) hashM

klog :: IO ()
klog = do
    refs <- getAllRefs
    commits <- getCommits refs
    (lists, v) <- runStateT (traverseCommits commits) Set.empty
    let parentLinks = foldl' reducer "" lists
    let dot = "digraph commits {\n" <> mconcat [ "\"" <> refname <> "\" [shape=note]\n\"" <> refname <> "\" -> \"" <> Utf8.toString referent <> "\"\n"
            | (refname, referent) <- refs ] <> (Utf8.toString parentLinks <> "\n}")
    (stdin, _, _, _) <- runInteractiveCommand "dot -Tjpg -o k.jpg"
    hPutStr stdin dot
    hClose stdin
    where
        reducer :: BS.ByteString -> ParsedObj -> BS.ByteString
        reducer acc  (ParsedCommit hash MkCommitHeaders{parent= pm} _) =
                case pm of
                    Nothing -> acc <> node
                    Just p -> acc <> node <> "\"" <> hash <> "\" -> \"" <> p <> "\"\n"
            where
                node = "\"" <> hash <> "\" [shape=box style=filled label=\"" <> BS.take 10 hash <> "\"]\n"
        reducer _ _ = ""
        getAllRefs :: IO [(String, BS.ByteString)]
        getAllRefs = do
            paths <- listFiles refsDir
            forM paths resolveRef
        -- (refname, referent)
        resolveRef :: FilePath -> IO (String, BS.ByteString)
        resolveRef path = do
            referent <- BS.readFile path
            pure (makeRelative refsDir path, referent)
        getCommits :: [(String, BS.ByteString)] -> IO [ParsedObj]
        getCommits refs = do
            let mapper = runMaybeT . getCommit . Utf8.toString . snd
            let actions = fmap mapper refs
            let action = sequence actions
            catMaybes <$> action

type Visited = Set BS.ByteString

-- 从这些commit出发，遍历所有可以到达的commit并返回（不重复遍历）
traverseCommits :: [ParsedObj] -> StateT Visited IO [ParsedObj]
traverseCommits objs = do
    visited <- get
    let newVisited = Set.union visited $ Set.fromList $ map parsedObjHash objs
    foldM (\os item -> do
            newO <- processCommit item
            pure $ os <> newO
            ) objs  objs


processCommit :: ParsedObj -> StateT Visited IO [ParsedObj]
processCommit (ParsedCommit hash MkCommitHeaders{parent=Just p} _) = do
    visited  <- get
    Just objP <- lift . runMaybeT $ getCommit $ Utf8.toString p
    if parsedObjHash objP `elem` visited then
        return []
    else do
        traverseCommits [objP]

processCommit _ = pure []

