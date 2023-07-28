{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Runner where

import Language.Docker
  ( Instruction (..),
    InstructionPos (InstructionPos, instruction),
    prettyPrint,
  )
import Metadata (DockerfileWithMetadata, Metadata (..), newMetadata, fixAllWithMetadata, injectAllWithMetadata)
import Options.Applicative
    ( header, progDesc, fullDesc, helper, (<**>), info, execParser )
import Data.Maybe (fromMaybe, maybe, Maybe)
import Smells (getSmellIds, Smell (smellId), getSmells, getSmellInjectors, PackageManagerPackageVersions (..), NpmVersions (NpmVersions), PipVersions (PipVersions), AptGetVersions (AptGetVersions), selectRandomSmells, ApkVersions (ApkVersions), GemVersions (GemVersions))
import ShellCheck.AST (Token)
import System.Random (StdGen, mkStdGen)
import qualified Data.Text.IO
import Data.Text (Text, pack, unpack)
import Options
import DockerfileParser (parseDockerfile)
import qualified Command.AptGet
import qualified Command.Pip
import qualified Command.Npm
import PackageVersions (loadPackageVersions, loadPackageVersionsInstallCommand)
import Data.Aeson (encode, encodeFile)
import qualified Data.ByteString.Lazy as BS (putStr, writeFile)
import Control.Monad (when)
import Dockerfile (Dockerfile, printDockerfile)
import Helper
import Fixer.PinBaseImageVersion (BaseImageVersions (BaseImageVersions), getBaseImages)
import qualified Command.Apk
import qualified Command.Gem
import Control.Applicative (Alternative(empty))
import Language.Docker (BaseImage (digest, tag, BaseImage, image), Instruction (From), Tag (Tag), Image (Image), image, imageName, instruction, unTag)
import Data.Maybe (mapMaybe, fromMaybe)
import Command (getCommandResult)
import Data.List.Split (splitOn)
import Data.Tuple (snd)

main :: IO ()
main = runProgramm =<< execParser opts
  where
    opts =
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "Inject and fix dockerfile security smells"
            <> header "dockercleaner - inject & fix Dockerfile security smells"
        )

createPreProcessMetadata :: InputFilePath -> Text -> Dockerfile -> Int -> Metadata
createPreProcessMetadata dockerfilePath dockerfileText dockerfile randomGenSeed =
  newMetadata
    { fileName = Just dockerfilePath,
      originalDockerfileHash = Just $ show $ hashDockerfile dockerfileText,
      originalDockerfile = Just $ unpack dockerfileText,
      originalDockerfileUglified = Just formatedDockerfile,
      originalDockerfileUglifiedHash = Just $ show $ hashDockerfile $ pack formatedDockerfile,
      Metadata.seed = Just randomGenSeed
    }
  where
    formatedDockerfile = printDockerfile dockerfile

processWithMetadata :: (DockerfileWithMetadata -> DockerfileWithMetadata) -> DockerfileWithMetadata -> DockerfileWithMetadata
processWithMetadata f dockerfile =
  ( resultDockerfile,
    metadata
      { processedDockerfile = Just $ printDockerfile resultDockerfile,
        processedDockerfileHash = Just $ show $ hashDockerfile $ pack $ printDockerfile resultDockerfile
      }
  )
  where
    (resultDockerfile, metadata) = f dockerfile

-- Get the base image of the given dockerfile
getBaseImagesWithTags :: [InstructionPos a] -> [(String, String)]
getBaseImagesWithTags = mapMaybe helper where
  helper InstructionPos {instruction=(From x@BaseImage {image = Image {imageName}, tag = Just Tag {unTag}})} = Just (Data.Text.unpack imageName, Data.Text.unpack unTag)
  helper InstructionPos {instruction=(From x@BaseImage {image = Image {imageName}, tag = Nothing})} = Just (Data.Text.unpack imageName, "latest")
  helper _ = Nothing

searchOSBaseImage :: (String, String) -> IO (String, String)
searchOSBaseImage baseImage = do
  result <- getCommandResult $ "python3 <path-to-dockercleaner>/package-versions/dso_crawler.py baseos -i " ++ fst baseImage ++ ":" ++ snd baseImage
  case result of
    Just res -> return $ toTuple (splitOn ":" res)
    Nothing -> return baseImage

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)

loadPackageManagerPackageVersions :: Dockerfile -> Maybe String -> IO PackageManagerPackageVersions
loadPackageManagerPackageVersions dockerfile modifiedDate = do
  let baseImage = head $ getBaseImagesWithTags dockerfile
  osImage <- searchOSBaseImage baseImage

  aptGetVersions <- AptGetVersions <$> loadPackageVersionsInstallCommand Command.AptGet.proxy "<path-to-dockercleaner>/package-versions/apt-packages.csv" osImage modifiedDate dockerfile
  apkVersions <- ApkVersions <$> loadPackageVersionsInstallCommand Command.Apk.proxy "<path-to-dockercleaner>/package-versions/apk-packages.csv" osImage modifiedDate dockerfile

  pipVersions <- PipVersions <$> loadPackageVersionsInstallCommand Command.Pip.proxy "<path-to-dockercleaner>/package-versions/pip-packages.csv" osImage modifiedDate dockerfile
  npmVersions <- NpmVersions <$> loadPackageVersionsInstallCommand Command.Npm.proxy "<path-to-dockercleaner>/package-versions/npm-packages.csv" osImage modifiedDate dockerfile
  gemVersions <- GemVersions <$> loadPackageVersionsInstallCommand Command.Gem.proxy "<path-to-dockercleaner>/package-versions/gem-packages.csv" osImage modifiedDate dockerfile
  return PackageManagerPackageVersions {aptGetVersions, pipVersions, npmVersions, apkVersions, gemVersions}

filterBySmellId :: (String -> Bool) -> [Smell] -> [Smell]
filterBySmellId f = filter (f . smellId)

chooseSmellInjectors :: Maybe String -> Bool -> StdGen -> Dockerfile -> [String] -> IO [Smell]
chooseSmellInjectors modifiedDate False _ dockerfile [] = do
  return getSmellInjectors
chooseSmellInjectors modifiedDate False randomGen dockerfile selectedSmells = filterBySmellId (`elem` selectedSmells) <$> chooseSmellInjectors modifiedDate False randomGen dockerfile []
chooseSmellInjectors modifiedDate True randomGen dockerfile selectedSmells = selectRandomSmells randomGen <$> chooseSmellInjectors modifiedDate False randomGen dockerfile selectedSmells

chooseSmellFixers :: Maybe String -> Bool -> StdGen -> Dockerfile -> [String] -> IO [Smell]
chooseSmellFixers modifiedDate False _ dockerfile [] = do
  let baseImage = head $ getBaseImagesWithTags dockerfile
  osImage <- searchOSBaseImage baseImage

  versions <- loadPackageManagerPackageVersions dockerfile modifiedDate
  return $ getSmells versions osImage

chooseSmellFixers modifiedDate False randomGen dockerfile selectedSmells = filterBySmellId (`elem` selectedSmells) <$> chooseSmellFixers modifiedDate False randomGen dockerfile []

runInjector :: [String] -> Bool -> Maybe Int -> Maybe String -> StdGen -> Dockerfile -> Metadata -> IO DockerfileWithMetadata
runInjector selectedSmells useRandomSmells seed modifiedDate randomGen dockerfile metadata = do
  smells <- chooseSmellInjectors modifiedDate useRandomSmells randomGen dockerfile selectedSmells
  return $ processWithMetadata (injectAllWithMetadata smells randomGen) (dockerfile, metadata)

runFixer :: [String] -> Bool -> Maybe Int -> Maybe String -> StdGen -> Dockerfile -> Metadata -> IO DockerfileWithMetadata
runFixer selectedSmells useRandomSmells seed modifiedDate randomGen dockerfile metadata = do
  smells <- chooseSmellFixers modifiedDate useRandomSmells randomGen dockerfile selectedSmells
  return $ processWithMetadata (fixAllWithMetadata smells) (dockerfile, metadata)

runInjectorFixer :: InputFilePath -> [String] -> Bool -> Bool -> Bool -> Maybe Int -> Maybe String -> IO DockerfileWithMetadata
runInjectorFixer input selectedSmells useRandomSmells injectSmells fixSmells seed modifiedDate = do
  dockerfileText <- Data.Text.IO.readFile input
  let Right dockerfile = parseDockerfile dockerfileText
  let randomGenSeed = fromMaybe (seedFromDockerfileText dockerfileText) seed
  let randomGen = mkStdGen randomGenSeed
  let metadata = createPreProcessMetadata input dockerfileText dockerfile randomGenSeed
  
  case (injectSmells, fixSmells) of
    (True, True) -> return $ processWithMetadata id (dockerfile, metadata)
    (True, False) -> runInjector selectedSmells useRandomSmells seed modifiedDate randomGen dockerfile metadata
    (False, True) -> runFixer selectedSmells useRandomSmells seed modifiedDate randomGen dockerfile metadata
    (False, False) -> return $ processWithMetadata id (dockerfile, metadata)

whenJust :: Maybe a -> (a -> IO ()) -> IO ()
whenJust (Just a) f = f a
whenJust Nothing _ = return ()

runProgramm :: Options -> IO ()
runProgramm
  Options
    { input,
      output,
      outputMetadataFile,
      logMetadata,
      smells,
      useRandomSmells,
      injectSmells,
      fixSmells,
      Options.seed = seed,
      modifiedDate
    } = do
    (dockerfile, metadata) <- runInjectorFixer input smells useRandomSmells injectSmells fixSmells seed modifiedDate
    whenJust output (\path -> writeFile path $ printDockerfile dockerfile)
    whenJust outputMetadataFile (\path -> encodeFile path metadata)
    when logMetadata $ BS.putStr $ encode metadata
