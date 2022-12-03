module Opt(
    CmdOpts(..),
    execParser
) where

import           Cmd
import           Data.Functor              ((<&>))
import           Options.Applicative       hiding (execParser)
import qualified Options.Applicative
import           Options.Applicative.Types (ReadM (ReadM))

data CmdOpts
  = Init
  | HashObject !HashObjectOpt
  | CatFile !CatFileOpt
  | WriteTree !WriteTreeOpt
  | ReadTree !ReadTreeOpt
  | Commit !CommitOpt
  | Log !LogOpt
  | Checkout !CheckoutOpt
  | Tag !TagOpt

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
    <> command "read-tree" (info readTreeCmdParser (progDesc "Read objects to repository."))
    <> command "commit" (info commitCmdParser (progDesc "Commit changes to repository."))
    <> command "log" (info logCmdParser (progDesc "Log commits."))
    <> command "checkout" (info checkoutCmdParser (progDesc "Checkout commit."))
    <> command "tag" (info tagCmdParser (progDesc "Tag hash."))
  )

initCmdParser :: Parser CmdOpts
initCmdParser = pure Init

hashObjectCmdParser :: Parser CmdOpts
hashObjectCmdParser = HashObject <$> argument (str <&> MkHashObjectOpt) (metavar "file")

catFileCmdParser :: Parser CmdOpts
catFileCmdParser = CatFile <$> argument (str <&> MkCatFileOpt) (metavar "hash")

writeTreeCmdParser :: Parser CmdOpts
writeTreeCmdParser = WriteTree <$> argument (str <&> MkWriteTreeOpt) (metavar "dir")

readTreeCmdParser :: Parser CmdOpts
readTreeCmdParser = ReadTree <$> argument (str <&> MkReadTreeOpt) (metavar "hash")

logCmdParser :: Parser CmdOpts
logCmdParser = Log <$> (pure MkEmptyLogOpt <|> argument (str <&> MkLogOpt) (metavar "hash"))

checkoutCmdParser :: Parser CmdOpts
checkoutCmdParser = Checkout <$> argument (str <&> MkCheckoutOpt) (metavar "hash")

tagCmdParser :: Parser CmdOpts
tagCmdParser = Tag . MkTagOpt <$> some (argument str (metavar "tagName [hash]"))

commitCmdParser :: Parser CmdOpts
commitCmdParser = Commit <$> commitOptParser
    where
        commitOptParser = MkCommitOpt <$> strOption (
               long "message"
            <> short 'm'
            <> metavar "message"
            <> help "Commit message" )

execParser :: IO CmdOpts
execParser = Options.Applicative.execParser parserInfo
