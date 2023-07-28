{-# LANGUAGE NamedFieldPuns #-}

module Command.Apk (Apk (ApkAdd, packages, unparsed), proxy) where

import Command (Command, InstallCommand, getPackages, loadLatestPackageVersion, mapPackages, parse, toAST, getCommandResult)
import CommandParser (Parameter (Flag, FlagWithParameter, Parameter), ParsedCommand (ParsedCommand), isFlag, parseCommand2, stringsToWordTokens)
import Control.Applicative (Alternative (many), (<|>))
import Data.List (intersect)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Proxy (Proxy (Proxy))
import Package (Package (Package, name, version), toPackage, toPackages)
import ShellCheck.AST (InnerToken (Inner_T_SimpleCommand), Token)
import Data.Either (rights, lefts)
import System.IO (hPutStr, stderr)

proxy :: Proxy Apk
proxy = Proxy

data Apk = ApkAdd
  { packages :: [Either Token Package],
    unparsed :: [Token]
  }
  deriving (Show)

instance Command Apk where
  parse = parseApkAdd

  toAST ApkAdd {unparsed, packages} = Inner_T_SimpleCommand [] (stringsToWordTokens command ++ lefts packages ++ unparsed)
    where
      command :: [String]
      command = "apk" : "add" : map printPackage (rights packages)
      printPackage :: Package -> String
      printPackage Package {name, version = ""} = name
      printPackage Package {name, version} = name ++ "=" ++ version

parseApkAdd :: InnerToken Token -> Maybe Apk
parseApkAdd t = commandToApk <$> parseCommand2 "apk" ["add"] flagsWithParameter t
  where
    flagsWithParameter = ["-t", "--virtual", "-p", "--root", "-X", "--repository", "--arch", "--cache-dir", "--cache-max-age", "--keys-dir", "--progress-fd", "--repositories-file", "--wait"]
    commandToApk (ParsedCommand ts) =
      ApkAdd
        { packages = findPackages ts,
          unparsed = unparsed ts
        }
      where
        findPackages :: [Parameter Token] -> [Either Token Package]
        findPackages = toPackages getName getVersion
          where
            getName text = takeWhile (not . isVersionSeparator) text
            getVersion text = drop 1 $ dropWhile (not . isVersionSeparator) text
            isVersionSeparator a = a == '='
        unparsed :: [Parameter Token] -> [Token]
        unparsed (Parameter _ : ts) = unparsed ts
        unparsed (FlagWithParameter flag param : ts) = flag : param : unparsed ts
        unparsed (Flag t : ts) = t : unparsed ts
        unparsed [] = []

instance InstallCommand Apk where
  getPackages ApkAdd {packages} = rights packages
  mapPackages f x@ApkAdd {packages} = x {packages = (map . fmap) f packages}
  loadLatestPackageVersion _ package = return (Just "")
