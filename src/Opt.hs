module Opt(
    CmdOpts(..),
    execParser
) where

import qualified Options.Applicative
import Options.Applicative hiding (execParser)
import Options.Applicative.Types (ReadM(ReadM))
import Data.Functor ((<&>))
import Cmd(HashObjectOpt(..), CatFileOpt(..), WriteTreeOpt(..))

data CmdOpts
  = Init
  | HashObject !HashObjectOpt
  | CatFile !CatFileOpt
  | WriteTree !WriteTreeOpt

parserInfo :: ParserInfo CmdOpts
parserInfo = info (helper <*> cmdParser)
  ( fullDesc
  <> header "hgit - DIY Git in Haskell" )

cmdParser :: Parser CmdOpts
cmdParser = hsubparser (
    command "init" (info initCmdParser (progDesc "Init repository"))
    <> command "hash-object" (info hashObjectCmdParser (progDesc "Hash object to repository."))
    <> command "cat-file" (info catFileCmdParser (progDesc "Cat file."))
    <> command "write-tree" (info writeTreeCmdParser (progDesc "Write objects to repository."))
  )

initCmdParser :: Parser CmdOpts
initCmdParser = pure Init

hashObjectCmdParser :: Parser CmdOpts
hashObjectCmdParser = HashObject <$> argument (str <&> MkHashObjectOpt) (metavar "file")

catFileCmdParser :: Parser CmdOpts
catFileCmdParser = CatFile <$> argument (str <&> MkCatFileOpt) (metavar "hash")

writeTreeCmdParser :: Parser CmdOpts
writeTreeCmdParser = WriteTree <$> argument (str <&> MkWriteTreeOpt) (metavar "dir")

execParser :: IO CmdOpts
execParser = Options.Applicative.execParser parserInfo