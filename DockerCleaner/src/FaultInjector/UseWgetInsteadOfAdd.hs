module FaultInjector.UseWgetInsteadOfAdd (injectFault) where

import Command (Command (toAST), maybeCommand, parse)
import Command.NoOp (NoOp (NoOp))
import Command.Wget (Wget (Wget, noVerbose, outputDocument, unparsed, url), proxy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes, isJust, maybeToList)
import Data.Text (pack)
import Debug.Trace (trace)
import DockerfileParser (createsNesting, fmapUnnestedTokens, mapUnnestedTokens)
import Language.Docker
  ( AddArgs (AddArgs),
    Chown (NoChown),
    CopyArgs (CopyArgs, sourceFlag),
    CopySource (NoSource),
    Instruction (Add, Copy, Env, Run),
    InstructionPos,
    SourcePath (SourcePath),
    TargetPath (TargetPath),
  )
import Language.Docker.Syntax (Chmod (NoChmod), RunArgs (RunArgs))
import ShellCheck.AST (InnerToken (..), Token)
import System.Random (RandomGen)
import Helper
import Dockerfile (Dockerfile)

-- Replace all wget commands that can be converted to ADD instructions by noops
removeReplaceableWgets :: InstructionPos (Maybe Token) -> InstructionPos (Maybe Token)
removeReplaceableWgets = fmap $ fmap $ fmapUnnestedTokens replaceWgetByNoop

-- Replace a wget command that can be converted to an ADD instruction by a noop
replaceWgetByNoop :: InnerToken Token -> InnerToken Token
replaceWgetByNoop t | isConvertableWget t = toAST NoOp
replaceWgetByNoop t = t

-- Is this token a wget command and can be converted to an ADD instruction?
isConvertableWget :: InnerToken Token -> Bool
isConvertableWget t = case parse t :: Maybe Wget of
  Just wget@Wget {} -> isJust $ wgetToAdd wget
  _ -> False

-- Creates an equivalent ADD instruction for a wget command.
wgetToAdd :: Wget -> Maybe (Instruction Token)
wgetToAdd
  Wget
    { url = Just url,
      outputDocument = Just outputDocument,
      unparsed = []
    } = Just $ Add $ AddArgs (SourcePath (pack url) :| []) (TargetPath $ pack outputDocument) NoChown NoChmod
wgetToAdd
  Wget
    { url = Just url,
      unparsed = []
    } = Just $ Add $ AddArgs (SourcePath (pack url) :| []) (TargetPath $ pack "./") NoChown NoChmod
wgetToAdd _ = Nothing

-- Creates an equivalent ADD instruction for a wget command.
createAddForWget :: InnerToken Token -> Maybe (InstructionPos (Maybe Token))
createAddForWget instruction = instructionToInstructionPos . fmap Just <$> maybeCommand proxy wgetToAdd instruction

-- Creates an equivalent new ADD instruction for every (unnested & simple) wget command.
createAddsForWgets :: InstructionPos (Maybe Token) -> Dockerfile
createAddsForWgets = catMaybes . (concatMap . concatMap . mapUnnestedTokens) createAddForWget

injectFault :: RandomGen g => g -> Dockerfile -> Dockerfile
injectFault _ = mapInstructionsToMany helper
  where
    helper :: InstructionPos (Maybe Token) -> Dockerfile
    helper instruction = createAddsForWgets instruction ++ [removeReplaceableWgets instruction]
