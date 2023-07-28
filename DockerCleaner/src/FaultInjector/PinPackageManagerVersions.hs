{-# LANGUAGE ScopedTypeVariables #-}

module FaultInjector.PinPackageManagerVersions where

import Command (InstallCommand, mapPackages)
import Data.Proxy (Proxy)
import Helper
import Language.Docker (Instruction, InstructionPos)
import Package (Package (version))
import ShellCheck.AST (Token)
import System.Random (RandomGen)
import Dockerfile (Dockerfile)

injectFault :: forall proxy a g. (InstallCommand a, RandomGen g) => proxy a -> g -> Dockerfile -> Dockerfile
injectFault _ _ = fmapDockerfileInstructions $ fmapCommands (mapPackages removeVersion :: (a -> a))
  where
    removeVersion package = package {version = ""}
