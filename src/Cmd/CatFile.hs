module Cmd.CatFile where
import Const (objectsDir)
import System.Directory (doesFileExist)
import Control.Monad (when)
import Cmd.CmdComm (preCheck)

data Opt = Opt String

catFile :: Opt -> IO ()
catFile (Opt hash) = preCheck $ do
    let file = objectsDir <> "/" <> hash
    exists <- doesFileExist file
    when exists $ readFile file >>= putStrLn
