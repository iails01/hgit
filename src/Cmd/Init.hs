module Cmd.Init(initRepo) where

import System.Directory(createDirectoryIfMissing)
import Prelude hiding (init)
import Const

initRepo :: IO ()
initRepo = do
  createDirectoryIfMissing False repoDir
