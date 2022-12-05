{-# LANGUAGE OverloadedStrings #-}

module Cmd
  ( CatFileOpt(..)
  , HashObjectOpt(..)
  , WriteTreeOpt(..)
  , ReadTreeOpt(..)
  , CommitOpt(..)
  , LogOpt(..)
  , CheckoutOpt(..)
  , TagOpt(..)
  , initRepo
  , catFile
  , hashObject
  , writeTree
  , readTree
  , commit
  , log
  , checkout
  , tag
  ) where

import qualified Data.ByteString.Char8 as Char8

import           System.Directory      (createDirectoryIfMissing,
                                        doesDirectoryExist)
import           System.Exit           (exitFailure)
import           System.IO             (hPutStrLn, stderr, stdout)

import qualified Base
import           Const
import qualified Data
import qualified Data.ByteString       as BS
import qualified Data.ByteString.UTF8  as Utf8
import Prelude hiding (log)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Base (resolveOid)

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
    obj <- Data.getObject hash
    maybe mempty (\(Data.MkObj _ content) -> Char8.putStrLn content) obj

newtype HashObjectOpt = MkHashObjectOpt String

hashObject :: HashObjectOpt -> IO ()
hashObject (MkHashObjectOpt file) = preCheck $ do
    content <- BS.readFile file
    hash <- Data.hashObject (Data.MkObj Data.Blob content)
    Char8.putStrLn ("saved: " <> hash)

data WriteTreeOpt = MkWriteTreeOpt FilePath

writeTree :: WriteTreeOpt -> IO ()
writeTree (MkWriteTreeOpt file) = preCheck $ do
    hash <- Base.writeTree file
    putStrLn (Utf8.toString hash)

data ReadTreeOpt = MkReadTreeOpt String

readTree :: ReadTreeOpt -> IO ()
readTree (MkReadTreeOpt oid) = preCheck $ do
    Base.readObj oid

data CommitOpt = MkCommitOpt String

commit :: CommitOpt -> IO ()
commit (MkCommitOpt msg) = preCheck $ do
    Base.commit msg

data LogOpt
    = MkEmptyLogOpt
    | MkLogOpt String

log :: LogOpt -> IO ()
log MkEmptyLogOpt = preCheck $ do
    Base.log "HEAD"
log (MkLogOpt oid) = preCheck $ do
    Base.log oid

data CheckoutOpt = MkCheckoutOpt String

checkout :: CheckoutOpt -> IO ()
checkout (MkCheckoutOpt oid) = preCheck $ do
    Base.checkout oid

data TagOpt = MkTagOpt [String]

tag :: TagOpt -> IO ()
tag (MkTagOpt []) = preCheck $ do
    hPutStrLn stderr "Tag name cannot be empty!"

tag (MkTagOpt [tagName]) = preCheck $ do
    Base.tag tagName "HEAD"

tag (MkTagOpt (tagName:oid:xs)) = preCheck $ do
    Base.tag tagName oid
