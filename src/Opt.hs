module Opt(
    CmdOpts(..),
    execParser
) where

import qualified Options.Applicative
import Options.Applicative hiding (execParser)

data CmdOpts = Init

parserInfo :: ParserInfo CmdOpts
parserInfo = info (helper <*> cmdParser)
  ( fullDesc
  <> header "hgit" )

cmdParser :: Parser CmdOpts
cmdParser = hsubparser (command "init" (info initCmdParser (progDesc "Init repository")))

initCmdParser :: Parser CmdOpts
initCmdParser = pure Init

execParser :: IO CmdOpts
execParser = Options.Applicative.execParser parserInfo