{-# LANGUAGE ScopedTypeVariables #-}

module Command (Command (parse, toAST), maybeCommand, fmapCommand, InstallCommand, getPackages, getPackagesFromToken, mapPackages, formatCommand, loadLatestPackageVersion, getCommandResult) where

import Language.Docker (InstructionPos)
import Package (Package)
import ShellCheck.AST (InnerToken, Token)
import System.Process (readCreateProcess, shell, readCreateProcessWithExitCode)
import Control.Monad (when, unless)
import GHC.IO.Exception (ExitCode(ExitFailure, ExitSuccess))
import System.IO (hPutStr, stderr)

class Command a where
  -- Parse the given token as the command
  parse :: InnerToken Token -> Maybe a

  -- Convert the command to a token
  toAST :: a -> InnerToken Token

-- Apply f to all tokens that can be parsed as the command
fmapCommand :: Command a => (a -> a) -> InnerToken Token -> InnerToken Token
fmapCommand f token = maybe token (toAST . f) (parse token)

-- Try to parse the token as the given command. On success evaluate f with the command as parameter.
maybeCommand :: forall proxy a b. (Command a) => proxy a -> (a -> Maybe b) -> InnerToken Token -> Maybe b
maybeCommand _ f token = f =<< parse token

-- Format all occurances of the command uniformly
formatCommand :: forall proxy a. (Command a) => proxy a -> InnerToken Token -> InnerToken Token
formatCommand _ = fmapCommand (id :: a -> a)

class Command a => InstallCommand a where
  -- Get the packages the given command might install
  getPackages :: a -> [Package]

  -- Apply f to all packages of the given command
  mapPackages :: (Package -> Package) -> a -> a

  loadLatestPackageVersion :: proxy a -> String -> IO (Maybe String)

-- Get the packages that might be installed by the given command when executing the givin token
getPackagesFromToken :: forall proxy a. (InstallCommand a) => proxy a -> InnerToken Token -> [Package]
getPackagesFromToken _ token = maybe [] (getPackages :: a -> [Package]) (parse token)

-- Get the result of a shell command execution.
getCommandResult :: String ->  IO (Maybe String)
getCommandResult command = do
  (exitcode, out, err) <- readCreateProcessWithExitCode (shell command) ""
  unless (exitcode == ExitSuccess) $ hPutStr stderr err
  return $ case exitcode of 
    ExitFailure _ -> Nothing
    ExitSuccess -> Just $ head $ lines out
