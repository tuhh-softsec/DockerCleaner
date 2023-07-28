{-# LANGUAGE ScopedTypeVariables #-}

module DockerfileParser where

import Command (Command, InstallCommand, fmapCommand)
import Data.Functor.Identity (Identity (..))
import Data.List (nub, intercalate)
import Data.Text (Text, pack, unpack)
import Helper
import Language.Docker
  ( Arguments,
    Check (Check, NoCheck),
    CopySource (NoSource),
    Instruction (..),
    InstructionPos (InstructionPos, instruction),
    parseFile,
    parseText
  )
import Language.Docker.Parser (Error)
import Language.Docker.Syntax ( InstructionPos )
import ShellCheck.AST (Id (..), InnerToken (..), Token (OuterToken))
import ShellCheck.Interface (mockedSystemInterface, newParseSpec, prRoot, psFilename, psScript)
import ShellCheck.Parser (parseScript)
import Dockerfile (Dockerfile)
import Data.Maybe (isJust, mapMaybe, catMaybes)
import Control.Monad (join)

foldUnnestedTokens :: (InnerToken Token -> a -> a) -> a -> Token -> a
foldUnnestedTokens = foldTokensUntil createsNesting

fmapUnnestedTokens :: (InnerToken Token -> InnerToken Token) -> Token -> Token
fmapUnnestedTokens = fmapTokensUntil createsNesting

concatUnnestedTokens :: Token -> [InnerToken Token]
concatUnnestedTokens = concatTokensUntil createsNesting

mapUnnestedTokens :: (InnerToken Token -> b) -> Token -> [b]
mapUnnestedTokens = mapTokensUntil createsNesting

-- Are the tokens within this token no longer part of a top level statement, eg. are the nested?
createsNesting :: InnerToken Token -> Bool
createsNesting Inner_TA_Assignment {} = True
createsNesting Inner_TA_Variable {} = True
createsNesting Inner_TA_Trinary {} = True
createsNesting Inner_TC_Or {} = True
createsNesting Inner_T_Arithmetic {} = True
createsNesting Inner_T_IndexedElement {} = True
createsNesting Inner_T_UnparsedIndex {} = True
createsNesting Inner_T_Assignment {} = True
createsNesting Inner_T_CaseExpression {} = True
createsNesting Inner_T_DollarArithmetic {} = True
createsNesting Inner_T_DollarBraced {} = True
createsNesting Inner_T_DollarBracket {} = True
createsNesting Inner_T_DollarDoubleQuoted {} = True
createsNesting Inner_T_DollarExpansion {} = True
createsNesting Inner_T_DollarSingleQuoted {} = True
createsNesting Inner_T_DollarBraceCommandExpansion {} = True
createsNesting Inner_T_ForArithmetic {} = True
createsNesting Inner_T_ForIn {} = True
createsNesting Inner_T_Function {} = True
createsNesting Inner_T_HereDoc {} = True
createsNesting Inner_T_HereString {} = True
createsNesting Inner_T_IfExpression {} = True
createsNesting Inner_T_OrIf {} = True
createsNesting Inner_T_ProcSub {} = True
createsNesting Inner_T_UntilExpression {} = True
createsNesting Inner_T_WhileExpression {} = True
createsNesting Inner_T_BatsTest {} = True
createsNesting _ = False

parseShell :: Text -> Maybe Token
parseShell = prRoot . unpackIdentity . parse . unpack
  where
    parse s = parseScript (mockedSystemInterface []) newParseSpec {psScript = s}
    unpackIdentity (Identity a) = a

parseDockerfile :: Text -> Either Error Dockerfile
parseDockerfile = fmap parseDockerfileShell . parseText
  where
    parseDockerfileShell :: [InstructionPos Text] -> Dockerfile
    parseDockerfileShell = map $ fmap parseShell
