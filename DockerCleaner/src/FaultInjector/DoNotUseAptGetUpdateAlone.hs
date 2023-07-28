module FaultInjector.DoNotUseAptGetUpdateAlone (injectFault) where

import Command (Command (parse, toAST), fmapCommand)
import Command.AptGet (AptGet (AptGetInstall, AptGetUpdate, noInstallRecommends), isAptGetInstall, isAptGetUpdate)
import Command.NoOp (NoOp (NoOp))
import Control.Monad (foldM)
import Data.Set (empty)
import Helper
import Language.Docker (Instruction (Run), InstructionPos (InstructionPos, instruction, lineNumber, sourcename))
import Language.Docker.Syntax (Arguments (ArgumentsText), RunArgs (RunArgs), RunFlags (RunFlags))
import ShellCheck.AST (Id (Id), InnerToken (Inner_T_Literal, Inner_T_SimpleCommand), Token (OuterToken))
import System.Random (RandomGen)
import Dockerfile (Dockerfile)

instructionWithoutAptGetUpdate :: InstructionPos (Maybe Token) -> InstructionPos (Maybe Token)
instructionWithoutAptGetUpdate = fmap $ fmap $ fmapToken replaceAptGetUpdateByNoop

replaceAptGetUpdateByNoop :: InnerToken Token -> InnerToken Token
replaceAptGetUpdateByNoop t | isAptGetUpdate t = toAST NoOp
replaceAptGetUpdateByNoop t = t

hasAptGetUpdateAndInstall :: InstructionPos (Maybe Token) -> Bool
hasAptGetUpdateAndInstall instruction = anyTokenOfInstruction isAptGetUpdate instruction && anyTokenOfInstruction isAptGetInstall instruction

injectFault :: RandomGen g => g -> Dockerfile -> Dockerfile
injectFault _ = mapInstructionsToMany helper
  where
    helper :: InstructionPos (Maybe Token) -> Dockerfile
    helper instruction | hasAptGetUpdateAndInstall instruction = [toInstruction $ AptGetUpdate [], instructionWithoutAptGetUpdate instruction]
    helper instruction = [instruction]
