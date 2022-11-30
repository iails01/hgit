{-# LANGUAGE OverloadedStrings #-}

module Base(writeTree) where
    
import Const (repoDir)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import Control.Monad (when, forM)
import Data (hashObject, getObjType, ObjType (..))
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as Lazy
import Data.List (sort, intercalate)
import Data.String ( IsString(fromString) )

writeTree :: FilePath -> IO Lazy.ByteString 
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
            pure (hash, fromString e, Tree)
        else do
            content <- Lazy.readFile path
            hash <- hashObject Blob content
            pure (hash, fromString e, Blob)
    let lines = map (\(hash, name, objType) -> hash <> " " <> name <> " " <> getObjType objType) (sort paths)
    let content = Lazy.intercalate "\n" lines
    hashObject Tree content

listFiles :: FilePath -> IO [FilePath]
listFiles root = do
  ds <- getDirectoryContents root
  paths <- forM (filter (`notElem` [".","..",repoDir]) ds) $ \e -> do
    let path = root </> e
    isDir <- doesDirectoryExist path
    if isDir then listFiles path
    else return [path]
  return (concat paths)