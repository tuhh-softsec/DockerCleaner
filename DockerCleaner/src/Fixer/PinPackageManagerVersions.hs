{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fixer.PinPackageManagerVersions where

import Command (InstallCommand, mapPackages)
import Data.Proxy (Proxy)
import Helper
import Language.Docker (Instruction, InstructionPos)
import Package (Package (Package, name, version))
import ShellCheck.AST (Token)
import Dockerfile (Dockerfile)
import Data.List (isSuffixOf)

fix :: forall proxy a. InstallCommand a => proxy a -> (String -> String) -> Dockerfile -> Dockerfile
fix _ getPackageVersion = fmapDockerfileInstructions $ fmapCommands (mapPackages addVersion :: a -> a)
  where
    addVersion package@Package {version = "", name} = package {version = getPackageVersion name}
    addVersion package = package
