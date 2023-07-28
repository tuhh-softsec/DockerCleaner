{-# LANGUAGE OverloadedStrings #-}

module Dockerfile where
  
import Language.Docker (InstructionPos, prettyPrint)
import ShellCheck.AST (Token)
import Data.Text (Text, pack)
import Data.Text.Lazy (unpack)
import PrintShellAST (printToken)
import qualified Command.AptGet
import qualified Command.Pip
import qualified Command.Npm
import qualified Command.Gem
import Command (formatCommand)
import Helper (fmapToken)

type Dockerfile = [InstructionPos (Maybe Token)]

formatCommands :: Token -> Token
formatCommands =
  fmapToken $
    formatCommand Command.AptGet.proxy
      . formatCommand Command.Pip.proxy
      . formatCommand Command.Npm.proxy
      . formatCommand Command.Gem.proxy

tokensDockerfileToTextDockerfile :: Dockerfile -> [InstructionPos Text]
tokensDockerfileToTextDockerfile = map $ fmap $ maybe "" $ (pack . printToken) . formatCommands

printDockerfile :: Dockerfile -> String
printDockerfile = unpack . prettyPrint . tokensDockerfileToTextDockerfile
