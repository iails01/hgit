{-# LANGUAGE OverloadedStrings #-}

module Data(hashObject, getObject, writeTree) where
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Text.Printf (printf)
import Crypto.Hash.SHA1 (hashlazy)
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist, getDirectoryContents)
import Const (objectsDir)
import Data.ByteString (ByteString)
import Control.Monad (when, forM)
import System.FilePath ((</>))


hashObject :: FilePath -> IO String
hashObject file = do
    let fileType = getFileType file
    content <- Lazy.readFile file
    let hash = toHexHash content
    createDirectoryIfMissing False objectsDir
    Lazy.writeFile (objectsDir </> hash) (fileType <> "\0" <> content)
    pure hash


getFileType :: FilePath -> Lazy.ByteString
getFileType _ = "blob"

toHexHash :: Lazy.ByteString -> String
toHexHash = toHex . hashlazy
  where
    toHex :: Strict.ByteString -> String
    toHex bytes = Strict.unpack bytes >>= printf "%02x"

test :: IO ()
test = do
    print $ toHexHash "this is cool" == "60f51187e76a9de0ff3df31f051bde04da2da891"

writeTree :: FilePath -> IO ()
writeTree dir = do
    exists <- doesDirectoryExist dir
    if exists then do
        files <- listFiles dir
        let actions = fmap hashObject files
        foldl (>>) (pure "") actions
        pure ()
    else pure ()

listFiles :: FilePath -> IO [FilePath]
listFiles root = do
  ds <- getDirectoryContents root
  paths <- forM (filter (`notElem` [".",".."]) ds) $ \e -> do
    let path = root </> e
    isDir <- doesDirectoryExist path
    if isDir then listFiles path
    else return [path]
  return (concat paths)

getObject :: String -> IO (Maybe ByteString)
getObject hash = do
    let file = objectsDir </> hash
    exists <- doesFileExist file
    if exists then do
        bs <- Strict.readFile file
        let (fileType, content) = Strict.breakSubstring "\0" bs
        pure $ Just content
    else pure Nothing