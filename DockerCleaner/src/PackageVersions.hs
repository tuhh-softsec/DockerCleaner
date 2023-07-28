{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module PackageVersions (loadPackageVersionsInstallCommand, loadPackageVersions, PackageVersions) where

import Command (InstallCommand, formatCommand, getPackages, loadLatestPackageVersion, getPackagesFromToken, getCommandResult)
import qualified Command.AptGet (proxy)
import qualified Command.Gem (proxy)
import qualified Command.Npm (proxy)
import qualified Command.Pip (proxy)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy as BSL
import Data.Csv (HasHeader (HasHeader), decode, encodeByName, header, namedField, namedRecord)
import Data.Functor ((<&>))
import Data.Map
    ( Map,
      findWithDefault,
      fromList,
      insertWith,
      member,
      toList,
      empty )
import Data.Proxy (Proxy)
import Data.String (IsString (fromString))
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO (readFile)
import qualified Data.Text.Lazy as L
import qualified Data.Vector (Vector, toList)
import Language.Docker
  ( Instruction (..),
    InstructionPos (InstructionPos, instruction),
    prettyPrint
  )
import Language.Docker.Syntax
  ( Arguments (ArgumentsText),
    RunArgs (RunArgs),
  )
import PrintShellAST (printToken)
import ShellCheck.AST (InnerToken, Token)
import System.Process (readCreateProcess, shell)
import ToJSON (tokenToJSONString)
import Data.List (nub, intercalate, isPrefixOf, isSuffixOf)
import Package (name)
import Helper (foldr3, foldTokens)
import Dockerfile (Dockerfile)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Either (rights)
import System.Directory (doesFileExist)
import Control.Monad (unless)

type PackageVersions = (Map String String)


-- Get the packages that might be installed by the given command when executing the given dockerfile
getUsedPackages :: forall proxy a. InstallCommand a => proxy a -> Dockerfile -> [String]
getUsedPackages command = nub . (foldr3 . flip . foldTokens) helper []
  where
    helper token acc = map name (getPackagesFromToken command token) ++ acc

createPackageVersionsCsv :: String -> IO ()
createPackageVersionsCsv filePath = writePackageVersionsToCsv filePath empty

-- Load data from a csv file that includes key-value pairs and has a header. Return these key-value pairs as a map.
loadPackageVersionsCsv :: String -> IO PackageVersions
loadPackageVersionsCsv filePath = do
  fileExists <- doesFileExist filePath
  unless fileExists (createPackageVersionsCsv filePath)
  file <- BS.readFile filePath
  let Right vector = decode HasHeader $ BSL.fromStrict file
  return $ Data.Map.fromList $ Data.Vector.toList vector

-- Write package versions to a csv file. To read these again {@see loadPackageVersionsCsv} can be used.
writePackageVersionsToCsv :: String -> Map String String -> IO ()
writePackageVersionsToCsv filePath versions = BSL.writeFile filePath csvData
  where
    csvData = encodeByName (header ["package" :: BS.ByteString, "version" :: BS.ByteString]) $ map (\(a, b) -> namedRecord [namedField "package" a, namedField "version" b]) $ toList versions

-- Load the latest versions of the packages of the given command.
loadLatestPackageVersions :: forall proxy a. (InstallCommand a) => proxy a -> [String] -> IO [(String, String)]
loadLatestPackageVersions command packages = do
  let filteredList = filter (notElem '/') packages
  versions <- mapM (loadLatestPackageVersion command) filteredList
  return $ mapMaybe helper $ zip filteredList versions where
    helper (a, Nothing) = Nothing
    helper (a, Just b) = Just (a, b)

getRealImageTag :: String -> String -> String
getRealImageTag imgName imgTag =
  case imgName of
    "ubuntu" -> getUbuntuCodeName imgTag
    "debian" -> getDebianCodeName imgTag
    "alpine" -> getAlpineLinuxCodeName imgTag
    _ -> imgTag

getAlpineLinuxCodeName :: String -> String
getAlpineLinuxCodeName version
  | "3" == version = "v3.17"
  | "3.0" `isPrefixOf` version = "v3.0"
  | "3.1" == version = "v3.1"
  | "3.2" `isPrefixOf` version = "v3.2"
  | "3.3" `isPrefixOf` version = "v3.3"
  | "3.4" `isPrefixOf` version = "v3.4"
  | "3.5" `isPrefixOf` version = "v3.5"
  | "3.6" `isPrefixOf` version = "v3.6"
  | "3.7" `isPrefixOf` version = "v3.7"
  | "3.8" `isPrefixOf` version = "v3.8"
  | "3.9" `isPrefixOf` version = "v3.9"
  | "3.10" `isPrefixOf` version = "v3.10"
  | "3.11" `isPrefixOf` version = "v3.11"
  | "3.12" `isPrefixOf` version = "v3.12"
  | "3.13" `isPrefixOf` version = "v3.13"
  | "3.14" `isPrefixOf` version = "v3.14"
  | "3.15" `isPrefixOf` version = "v3.15"
  | "3.16" `isPrefixOf` version = "v3.16"
  | "3.17" `isPrefixOf` version = "v3.17"
  | otherwise = version

getUbuntuCodeName :: String -> String
getUbuntuCodeName version
  | "14.04" `isPrefixOf` version = "trusty"
  | "16.04" `isPrefixOf` version = "xenial"
  | "18.04" `isPrefixOf` version = "bionic"
  | "20.04" `isPrefixOf` version = "focal"
  | "22.04" `isPrefixOf` version = "jammy"
  | "22.10" `isPrefixOf` version = "kinetic"
  | "23.04" `isPrefixOf` version = "lunar"
  | otherwise = version

getDebianCodeName :: String -> String
getDebianCodeName version
  | "buster" `isPrefixOf` version = "buster" -- version defined in Dockerfile could be buster or buster-slim
  | "bullseye" `isPrefixOf` version = "bullseye"
  | "bookworm" `isPrefixOf` version = "bookworm"
  | "10.13" `isPrefixOf` version = "Debian10.13"
  | "11.6" `isPrefixOf` version = "Debian11.6"
  | "10" == version || "10-slim" == version = "buster"
  | "11" == version || "11-slim" == version = "bullseye"
  | otherwise = version

-- Load the package versions of the provided packages. Package versions are loaded from the given csv file or, when no data about that package exists in the csv file, the latest package version is loaded.
loadPackageVersions :: ([String] -> IO [(String, String)]) -> String -> [String] -> (String, String) -> Maybe String -> Dockerfile -> IO PackageVersions
loadPackageVersions loadLatestPackageVersions csvFile [] baseImage modifiedDate dockerfile = do
  return empty

loadPackageVersions loadLatestPackageVersions csvFile usedPackages baseImage modifiedDate dockerfile = do
  let realImgTag = uncurry getRealImageTag baseImage
  getCommandResult $ "python3 <path-to-dockercleaner>/package-versions/get_package_versions.py " ++ realImgTag ++ " " ++ fromMaybe "1900-01-01" modifiedDate ++ " " ++ csvFile ++ " " ++ intercalate "," usedPackages

  csvPackageVersions <- loadPackageVersionsCsv "./package-versions.csv"
  fetchedPackageVersions <- loadLatestPackageVersions $ filter (\package -> not (member package csvPackageVersions)) usedPackages
  return $ foldr insertPackageVersion csvPackageVersions fetchedPackageVersions
  where
    insertPackageVersion (package, version) = insertWith (\new old -> if old == "" then new else old) package version


addMissingPackages :: PackageVersions -> [String] -> PackageVersions
addMissingPackages = foldr insertPackageVersion
  where
    insertPackageVersion package = insertWith (\new old -> if old == "" then new else old) package ""


-- Load the package versions of the packages installed by the command in the given Dockerfile. Package versions are loaded from the given csv file or.
-- When no data about that package exists in the csv file, the latest package version is loaded. The newly loaded package versions are then also saved to the csv file.
loadPackageVersionsInstallCommand :: forall proxy a. (InstallCommand a) => proxy a -> String -> (String, String) -> Maybe String -> Dockerfile -> IO PackageVersions
loadPackageVersionsInstallCommand command csvFile baseImage modifiedDate dockerfile = loadPackageVersions (loadLatestPackageVersions command) csvFile (getUsedPackages command dockerfile) baseImage modifiedDate dockerfile