module Const where
import System.FilePath ((</>))

repoDir :: FilePath
repoDir = ".hgit"

objectsDir :: FilePath
objectsDir = repoDir </> "objects"

refsDir :: FilePath
refsDir = repoDir </> "refs"

headFile :: FilePath
headFile = repoDir </> "HEAD"

spaceChar :: (Num a) => a
spaceChar = 32