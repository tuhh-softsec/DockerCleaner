{-# LANGUAGE NamedFieldPuns #-}

module Command.Pip (Pip (PipInstall, packages, unparsed), proxy) where

import Command (Command, InstallCommand, getCommandResult, getPackages, loadLatestPackageVersion, mapPackages, parse, toAST)
import CommandParser (Parameter (Flag, FlagWithParameter, Parameter), ParsedCommand (ParsedCommand), addWhen, parseCommand2, stringsToWordTokens)
import Control.Applicative ((<|>))
import Data.List (intersect)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (Proxy))
import Package (Package (Package, name, version), toPackage, toPackages)
import ShellCheck.AST (InnerToken (Inner_T_SimpleCommand), Token)
import Data.Either (rights, lefts)

proxy :: Proxy Pip
proxy = Proxy

data Pip = PipInstall
  { packages :: [Either Token Package],
    unparsed :: [Token]
  }
  deriving (Show)

instance Command Pip where
  parse = parsePipInstall

  toAST PipInstall {unparsed, packages} = Inner_T_SimpleCommand [] (stringsToWordTokens command ++ lefts packages ++ unparsed)
    where
      command :: [String]
      command = "pip" : "install" : map printPackage (rights packages)
      printPackage :: Package -> String
      printPackage Package {name, version = ""} = name
      printPackage Package {name, version} = name ++ '=' : '=' : version

parsePipInstall :: InnerToken Token -> Maybe Pip
parsePipInstall t = commandToPip <$> parseCommand2 "pip" ["install"] flagsWithParameter t
  where
    flagsWithParameter =
      [ "-e",
        "--editable",
        "-r",
        "--requirement",
        "-b",
        "--build",
        "-t",
        "--target",
        "-d",
        "--download",
        "--download-cache",
        "--src",
        "--install-option",
        "--global-option",
        "--root",
        "-i",
        "--index-url",
        "--extra-index-url",
        "-f",
        "--find-links",
        "--allow-external",
        "--allow-unverified"
      ]
    commandToPip (ParsedCommand ts) =
      PipInstall
        { packages = findPackages ts,
          unparsed = unparsed ts
        }
      where
        findPackages :: [Parameter Token] -> [Either Token Package]
        findPackages = toPackages getName getVersion
          where
            getName text = takeWhile (not . isVersionSeparator) text
            getVersion text = drop 2 $ dropWhile (not . isVersionSeparator) text
            isVersionSeparator a = a == '='
        unparsed :: [Parameter Token] -> [Token]
        unparsed (Parameter _ : ts) = unparsed ts
        unparsed (FlagWithParameter flag param : ts) = flag : param : unparsed ts
        unparsed (Flag t : ts) = t : unparsed ts
        unparsed [] = []

instance InstallCommand Pip where
  getPackages PipInstall {packages} = rights packages
  mapPackages f x@PipInstall {packages} = x {packages = (map . fmap) f packages}
  loadLatestPackageVersion _ package = getCommandResult $ "curl -s https://pypi.org/pypi/" ++ package ++ "/json | jq -r '.info.version'"
