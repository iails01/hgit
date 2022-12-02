module Const where
import System.FilePath ((</>))

repoDir :: String
repoDir = ".hgit"

objectsDir :: String
objectsDir = repoDir </> "objects"

headFile :: String
headFile = repoDir </> "HEAD"