{-# LANGUAGE OverloadedStrings #-}

module Cmd(CatFileOpt(..), HashObjectOpt(..), WriteTreeOpt(..), initRepo, catFile, hashObject, writeTree) where

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
    content <- getObject hash
    maybe (pure ()) Char8.putStrLn content

newtype HashObjectOpt = MkHashObjectOpt String

hashObject :: HashObjectOpt -> IO ()
hashObject (MkHashObjectOpt file) = preCheck $ do
    content <- BS.readFile file
    hash <- Data.hashObject Data.Blob content
    Char8.putStrLn ("saved: " <> hash)

data WriteTreeOpt = MkWriteTreeOpt FilePath

writeTree :: WriteTreeOpt -> IO ()
writeTree (MkWriteTreeOpt file) = preCheck $ do
    Base.writeTree file
    pure ()