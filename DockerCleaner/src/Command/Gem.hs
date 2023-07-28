{-# LANGUAGE NamedFieldPuns #-}

module Command.Gem (Gem (GemInstall, package, unparsed), proxy) where

import Command (Command, InstallCommand, getPackages, loadLatestPackageVersion, mapPackages, parse, toAST, getCommandResult)
import CommandParser (Parameter (Flag, FlagWithParameter, Parameter), ParsedCommand (ParsedCommand), Tokenlike (toInnerToken, getSimpleText), isFlag, parseCommand2, stringsToWordTokens)
import Control.Applicative (Alternative (many), (<|>))
import Data.List (intersect)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Proxy (Proxy (Proxy))
import Package (Package (Package, name, version))
import ShellCheck.AST (InnerToken (Inner_T_SimpleCommand), Token)

proxy :: Proxy Gem
proxy = Proxy

data Gem = GemInstall
  { package :: Package,
    unparsed :: [Token]
  }
  deriving (Show)

instance Command Gem where
  parse = parseGemInstall

  toAST GemInstall {unparsed, package} = Inner_T_SimpleCommand [] (stringsToWordTokens command ++ unparsed)
    where
      command :: [String]
      command = "gem" : "install" : printPackage package
      printPackage :: Package -> [String]
      printPackage Package {name, version = ""} = [name]
      printPackage Package {name, version} = [name, "--version", version]

parseGemInstall :: InnerToken Token -> Maybe Gem
parseGemInstall t = commandToGem <$> parseCommand2 "gem" ["install", "i"] flagsWithParameter t
  where
    versionFlags = ["--version", "-v"]
    flagsWithParameter = versionFlags ++ ["--platform", "--document", "--build-root", "--file", "-g", "--without", "-p", "--http-proxy", "--no-http-proxy", "--config-file"]
    commandToGem (ParsedCommand ts) =
      GemInstall
        { package = Package {name = fromMaybe "" $ findPackageName ts, version = fromMaybe "" $ findPackageVersion ts},
          unparsed = unparsed ts
        }
      where
        isVersionFlag :: Token -> Bool
        isVersionFlag t = any (`isFlag` t) versionFlags
        findPackageName :: [Parameter Token] -> Maybe String
        findPackageName (Parameter t : _) = getSimpleText $ toInnerToken t
        findPackageName (t : ts) = findPackageName ts
        findPackageName [] = Nothing
        findPackageVersion :: [Parameter Token] -> Maybe String
        findPackageVersion (FlagWithParameter flag param : ts) | isVersionFlag flag = getSimpleText $ toInnerToken param
        findPackageVersion (t : ts) = findPackageVersion ts
        findPackageVersion [] = Nothing
        unparsed :: [Parameter Token] -> [Token]
        unparsed (FlagWithParameter flag param : ts) | isVersionFlag flag = unparsed ts
        unparsed (FlagWithParameter flag param : ts) = flag : param : unparsed ts
        unparsed (Parameter _ : ts) = unparsed ts
        unparsed (Flag t : ts) = t : unparsed ts
        unparsed [] = []

instance InstallCommand Gem where
  getPackages GemInstall {package} = [package]
  mapPackages f x@GemInstall {package} = x {package = f package}
  loadLatestPackageVersion _ package = getCommandResult $ "curl --silent https://rubygems.org/api/v1/versions/" ++ package ++ "/latest.json | jq -r '.version'"
