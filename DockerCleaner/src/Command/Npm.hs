{-# LANGUAGE NamedFieldPuns #-}

module Command.Npm (Npm (NpmInstall, packages, unparsed), proxy) where

import Command (Command, InstallCommand, getCommandResult, getPackages, loadLatestPackageVersion, mapPackages, parse, toAST)
import CommandParser (Parameter (Flag, FlagWithParameter, Parameter), ParsedCommand (ParsedCommand), parseCommand2, stringsToWordTokens)
import Control.Applicative ((<|>))
import Data.List (intersect)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (Proxy))
import Package (Package (Package, name, version), toPackage, toPackages)
import ShellCheck.AST (InnerToken (Inner_T_SimpleCommand), Token)
import Data.Either (rights, lefts)

proxy :: Proxy Npm
proxy = Proxy

data Npm = NpmInstall
  { packages :: [Either Token Package],
    unparsed :: [Token]
  }
  deriving (Show)

instance Command Npm where
  parse = parseNpmInstall

  toAST NpmInstall {unparsed, packages} = Inner_T_SimpleCommand [] (stringsToWordTokens command ++ lefts packages ++ unparsed)
    where
      command :: [String]
      command = "npm" : "install" : map printPackage (rights packages)
      printPackage :: Package -> String
      printPackage Package {name, version = ""} = name
      printPackage Package {name, version} = name ++ '@' : version

parseNpmInstall :: InnerToken Token -> Maybe Npm
parseNpmInstall t = commandToNpm <$> parseCommand2 "npm" aliases flagsWithParameter t
  where
    aliases = ["install", "i", "in", "ins", "inst", "insta", "instal", "isnt", "isnta", "isntal", "add"]
    flagsWithParameter = ["--omit", "-w", "--workspace"]
    commandToNpm (ParsedCommand ts) =
      NpmInstall
        { packages = findPackages ts,
          unparsed = unparsed ts
        }
      where
        findPackages :: [Parameter Token] -> [Either Token Package]
        findPackages = toPackages getName getVersion
          where
            getName ('@' : text) = '@' : takeWhile (not . isVersionSeparator) text
            getName text = takeWhile (not . isVersionSeparator) text
            getVersion ('@' : text) = drop 1 $ dropWhile (not . isVersionSeparator) $ tail text
            getVersion text = drop 1 $ dropWhile (not . isVersionSeparator) text
            isVersionSeparator a = a == '@'
        unparsed :: [Parameter Token] -> [Token]
        unparsed (Parameter _ : ts) = unparsed ts
        unparsed (FlagWithParameter flag param : ts) = flag : param : unparsed ts
        unparsed (Flag t : ts) = t : unparsed ts
        unparsed [] = []

instance InstallCommand Npm where
  getPackages NpmInstall {packages} = rights packages
  mapPackages f x@NpmInstall {packages} = x {packages = (map . fmap) f packages}
  loadLatestPackageVersion _ package = getCommandResult $ "curl --silent -H \"Accept: application/vnd.npm.install-v1+json\" https://registry.npmjs.org/" ++ package ++ " | jq -r '.[\"dist-tags\"].latest'"
