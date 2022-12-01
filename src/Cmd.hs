{-# LANGUAGE OverloadedStrings #-}

module Cmd(CatFileOpt(..), HashObjectOpt(..), WriteTreeOpt(..), ReadTreeOpt(..), initRepo, catFile, hashObject, writeTree, readTree) where

import qualified Data.ByteString.Char8 as Char8
import Data (getObject,)

import System.Directory
    ( createDirectoryIfMissing,
      doesDirectoryExist,
      createDirectoryIfMissing )
import System.IO (hPutStrLn, stderr, stdout)
import System.Exit (exitFailure)

import Const
import qualified Data
import qualified Base
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as Utf8

preCheck :: IO a -> IO a
preCheck action = do
    repoExists <- doesDirectoryExist repoDir
    if repoExists then do
        action
    else do
        hPutStrLn stderr "Repository not initialized!"
        exitFailure

initRepo :: IO ()
initRepo = do
  createDirectoryIfMissing False repoDir

data CatFileOpt = MkCatFileOpt String

catFile :: CatFileOpt -> IO ()
catFile (MkCatFileOpt hash) = preCheck $ do
    obj <- getObject hash
    maybe mempty (\(Data.MkObj _ content) -> Char8.putStrLn content) obj

newtype HashObjectOpt = MkHashObjectOpt String

hashObject :: HashObjectOpt -> IO ()
hashObject (MkHashObjectOpt file) = preCheck $ do
    content <- BS.readFile file
    hash <- Data.hashObject Data.Blob content
    Char8.putStrLn ("saved: " <> hash)

data WriteTreeOpt = MkWriteTreeOpt FilePath

writeTree :: WriteTreeOpt -> IO ()
writeTree (MkWriteTreeOpt file) = preCheck $ do
    hash <- Base.writeTree file
    putStrLn (Utf8.toString hash)

data ReadTreeOpt = MkReadTreeOpt String

readTree :: ReadTreeOpt -> IO ()
readTree (MkReadTreeOpt hash) = preCheck $ do
    Base.readObj hash