{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Fixer.UseCopyInsteadOfAdd where

import Command.AptGet (AptGet (AptGetInstall, noInstallRecommends, packages, unparsed))
import Command.Wget (Wget (Wget, noVerbose, outputDocument, unparsed, url))
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.Text (isPrefixOf, unpack)
import Helper
import Language.Docker (AddArgs (AddArgs, chmodFlag, chownFlag, sourcePaths, targetPath), CopyArgs (CopyArgs, sourceFlag), CopySource (NoSource), Instruction (Add, Copy, Run), InstructionPos (InstructionPos), SourcePath (SourcePath), TargetPath (TargetPath))
import Language.Docker.Syntax (Arguments (ArgumentsList), RunArgs (RunArgs))
import Package (Package (Package, name, version))
import ShellCheck.AST (Id (Id), InnerToken (..), Token (OuterToken))
import ShellCheck.ASTLib (arguments)
import Dockerfile (Dockerfile)

addArgsToCopyArgs :: AddArgs -> CopyArgs
addArgsToCopyArgs (AddArgs sourcePaths targetPath chownFlag chmodFlag) = CopyArgs sourcePaths targetPath chownFlag chmodFlag NoSource

isFilePath :: SourcePath -> Bool
isFilePath (SourcePath path) = not ("http" `isPrefixOf` path)

isUrlPath :: SourcePath -> Bool
isUrlPath (SourcePath path) = True

sourcePathToString :: SourcePath -> String
sourcePathToString (SourcePath text) = unpack text

targetPathToString :: TargetPath -> String
targetPathToString (TargetPath text) = unpack text

-- which wget &> /dev/null || apt-get install wget=1.20.3
installWgetWhenNotInstalled :: Token
installWgetWhenNotInstalled = OuterToken (Id 15) (Inner_T_OrIf (OuterToken (Id 14) (Inner_T_Pipeline [] [OuterToken (Id 12) (Inner_T_Redirecting [OuterToken (Id 11) (Inner_T_FdRedirect "&" (OuterToken (Id 10) (Inner_T_IoFile (OuterToken (Id 7) Inner_T_Greater) (OuterToken (Id 9) (Inner_T_NormalWord [OuterToken (Id 8) (Inner_T_Literal "/dev/null")])))))] (OuterToken (Id 13) (Inner_T_SimpleCommand [] [OuterToken (Id 4) (Inner_T_NormalWord [OuterToken (Id 3) (Inner_T_Literal "which")]), OuterToken (Id 6) (Inner_T_NormalWord [OuterToken (Id 5) (Inner_T_Literal "wget")])])))])) (toPipeline (AptGetInstall {packages = [Right Package {name = "wget", version = ""}], noInstallRecommends = True, Command.AptGet.unparsed = []})))

instructionsForSourcePath :: AddArgs -> SourcePath -> Instruction (Maybe Token)
instructionsForSourcePath args path | isFilePath path = Copy $ addArgsToCopyArgs args {sourcePaths = path :| []}
instructionsForSourcePath AddArgs {targetPath} path | isUrlPath path = toRunInstruction $ Just $ toBashScript [installWgetWhenNotInstalled, toPipeline $ Wget {url = Just $ sourcePathToString path, outputDocument = Just $ targetPathToString targetPath, noVerbose = True, Command.Wget.unparsed = []}]
instructionsForSourcePath args path = Add $ args {sourcePaths = path :| []}

fix :: Dockerfile -> Dockerfile
fix = mapInstructionsToMany helper
  where
    helper :: InstructionPos (Maybe Token) -> Dockerfile
    helper (InstructionPos (Add args@AddArgs {sourcePaths}) _ _) | all isFilePath sourcePaths = [instructionToInstructionPos . fmap Just $ Copy $ addArgsToCopyArgs args]
    helper (InstructionPos (Add args@AddArgs {sourcePaths}) _ _) = toList $ fmap (instructionToInstructionPos . instructionsForSourcePath args) sourcePaths
    helper x = [x]
