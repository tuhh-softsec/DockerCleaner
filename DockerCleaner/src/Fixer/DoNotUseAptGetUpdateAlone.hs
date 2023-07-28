module Fixer.DoNotUseAptGetUpdateAlone (fix) where

import Command (Command (parse, toAST), fmapCommand)
import Command.AptGet (AptGet (AptGetInstall, AptGetUpdate, noInstallRecommends, unparsed), isAptGetInstall, isAptGetUpdate)
import Command.NoOp (NoOp (NoOp))
import Control.Monad (foldM)
import Data.Set (empty)
import Helper
import Language.Docker (Instruction (Run), InstructionPos (InstructionPos, instruction, lineNumber, sourcename))
import Language.Docker.Syntax (Arguments (ArgumentsText), RunArgs (RunArgs), RunFlags (RunFlags))
import ShellCheck.AST (Id (Id), InnerToken (Inner_T_Literal, Inner_T_SimpleCommand, Inner_T_Subshell), Token (OuterToken))
import Dockerfile (Dockerfile)

instructionWithoutAptGetUpdate :: InstructionPos (Maybe Token) -> InstructionPos (Maybe Token)
instructionWithoutAptGetUpdate = fmap $ fmap $ fmapToken replaceAptGetUpdateByNoop

replaceAptGetUpdateByNoop :: InnerToken Token -> InnerToken Token
replaceAptGetUpdateByNoop t | isAptGetUpdate t = toAST NoOp
replaceAptGetUpdateByNoop t = t

isAloneAptGetUpdateInstruction :: InstructionPos (Maybe Token) -> Bool
isAloneAptGetUpdateInstruction instruction = anyTokenOfInstruction isAptGetUpdate instruction && not (anyTokenOfInstruction isAptGetInstall instruction)

addAptGetUpdateToAptGetInstall :: AptGet -> InnerToken Token
addAptGetUpdateToAptGetInstall command = Inner_T_Subshell [toPipeline AptGetUpdate {unparsed = []}, toPipeline command]

addAptGetUpdateToAllAptGetInstalls :: InnerToken Token -> InnerToken Token
addAptGetUpdateToAllAptGetInstalls t = case (parse t :: Maybe AptGet) of
  Just x@AptGetInstall {} -> addAptGetUpdateToAptGetInstall x
  _ -> t

fix :: Dockerfile -> Dockerfile
fix instructions | any isAloneAptGetUpdateInstruction instructions = map helper instructions
  where
    helper :: InstructionPos (Maybe Token) -> InstructionPos (Maybe Token)
    helper instruction | isAloneAptGetUpdateInstruction instruction = instructionWithoutAptGetUpdate instruction
    helper instruction = fmap (fmap $ fmapToken addAptGetUpdateToAllAptGetInstalls) instruction
fix instructions = instructions
