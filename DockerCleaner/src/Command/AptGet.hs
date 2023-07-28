{-# LANGUAGE NamedFieldPuns #-}

module Command.AptGet (AptGet (AptGetInstall, AptGetUpdate, noInstallRecommends, packages, unparsed), proxy, isAptGetInstall, isAptGetUpdate) where

import Command (Command, InstallCommand (getPackages), getCommandResult, loadLatestPackageVersion, mapPackages, parse, toAST)
import CommandParser (Parameter (Flag, FlagWithParameter, Parameter), ParsedCommand (ParsedCommand), addWhen, isFlag, parseCommand2, stringsToWordTokens)
import Control.Applicative ((<|>))
import Data.List (find, isPrefixOf)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Proxy (Proxy (Proxy))
import Package (Package (Package, name, version), toPackage, toPackages)
import ShellCheck.AST (InnerToken (Inner_T_SimpleCommand), Token)
import System.Process (readCreateProcess, shell, readCreateProcessWithExitCode)
import Control.Monad (when)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import Data.Either (rights, lefts)

proxy :: Proxy AptGet
proxy = Proxy

data AptGet
  = AptGetInstall
      { noInstallRecommends :: Bool,
        packages :: [Either Token Package],
        unparsed :: [Token]
      }
  | AptGetUpdate
      { unparsed :: [Token]
      }
  deriving (Show)

instance Command AptGet where
  parse x = parseAptGetInstall x <|> parseAptGetUpdate x

  toAST AptGetInstall {noInstallRecommends, unparsed, packages} = Inner_T_SimpleCommand [] (stringsToWordTokens command ++ lefts packages ++ unparsed)
    where
      command :: [String]
      command = "apt-get" : "install" : addWhen noInstallRecommends "--no-install-recommends" (map printPackage (rights packages))
      printPackage :: Package -> String
      printPackage Package {name, version = ""} = name
      printPackage Package {name, version} = name ++ '=' : version
  toAST AptGetUpdate {unparsed} = Inner_T_SimpleCommand [] (stringsToWordTokens command ++ unparsed)
    where
      command = ["apt-get", "update"]

parseAptGetInstall :: InnerToken Token -> Maybe AptGet
parseAptGetInstall t = commandToAptGet <$> parseCommand2 "apt-get" ["install"] [] t
  where
    commandToAptGet (ParsedCommand ts) =
      AptGetInstall
        { noInstallRecommends = hasNoInstallRecommends ts,
          packages = findPackages ts,
          unparsed = unparsed ts
        }
      where
        isNoInstallRecommendsFlag = isFlag "--no-install-recommends"
        hasNoInstallRecommends :: [Parameter Token] -> Bool
        hasNoInstallRecommends (Flag t : ts) | isNoInstallRecommendsFlag t = True
        hasNoInstallRecommends (_ : ts) = hasNoInstallRecommends ts
        hasNoInstallRecommends [] = False
        findPackages :: [Parameter Token] -> [Either Token Package]
        findPackages = toPackages getName getVersion
          where
            getName text = takeWhile (not . isVersionSeparator) text
            getVersion text = drop 1 $ dropWhile (not . isVersionSeparator) text
            isVersionSeparator a = a == '='
        unparsed :: [Parameter Token] -> [Token]
        unparsed (Parameter _ : ts) = unparsed ts
        unparsed (FlagWithParameter flag param : ts) = flag : param : unparsed ts
        unparsed (Flag t : ts) | isNoInstallRecommendsFlag t = unparsed ts
        unparsed (Flag t : ts) = t : unparsed ts
        unparsed [] = []

parseAptGetUpdate :: InnerToken Token -> Maybe AptGet
parseAptGetUpdate t = commandToAptGet <$> parseCommand2 "apt-get" ["update"] [] t
  where
    commandToAptGet (ParsedCommand ts) =
      AptGetUpdate {unparsed = unparsed ts}
      where
        unparsed :: [Parameter Token] -> [Token]
        unparsed (Parameter t : ts) = t : unparsed ts
        unparsed (FlagWithParameter flag param : ts) = flag : param : unparsed ts
        unparsed (Flag t : ts) = t : unparsed ts
        unparsed [] = []

instance InstallCommand AptGet where
  getPackages AptGetInstall {packages} = rights packages
  getPackages _ = []
  mapPackages f x@AptGetInstall {packages} = x {packages = (map . fmap) f packages}
  mapPackages _ x = x
  loadLatestPackageVersion _ package = return (Just "")

isAptGetUpdate :: InnerToken Token -> Bool
isAptGetUpdate t = case parse t :: Maybe AptGet of
  Just AptGetUpdate {} -> True
  _ -> False

isAptGetInstall :: InnerToken Token -> Bool
isAptGetInstall t = case parse t :: Maybe AptGet of
  Just AptGetInstall {} -> True
  _ -> False
