module Options where

import Options.Applicative
import Smells (getSmellIds)

type InputFilePath = String

type OutputFilePath = String

type OutputMetadataFilePath = String

data Options = Options
  { input :: InputFilePath,
    output :: Maybe OutputFilePath,
    outputMetadataFile :: Maybe OutputMetadataFilePath,
    logMetadata :: Bool,
    smells :: [String],
    useRandomSmells :: Bool,
    injectSmells :: Bool,
    fixSmells :: Bool,
    seed :: Maybe Int,
    modifiedDate :: Maybe String
  }

options :: Parser Options
options =
  Options
    <$> strOption
      ( long "input"
          <> short 'i'
          <> metavar "INPUT_DOCKERFILE"
          <> action "file"
      )
    <*>  optional (strOption
      ( long "output"
          <> short 'o'
          <> metavar "OUTPUT_DOCKERFILE"
          <> action "file"
      ))
    <*> optional (strOption
      ( long "metadata-file"
          <> metavar "OUTPUT_METADATAFILE"
          <> action "file"
      ))
    <*> switch
      ( long "metadata"
          <> short 'm'
          <> help "Should metadata be generated and send to stdout"
      )
    <*>
      many (strOption (
        long "smell"
          <> short 's'
          <> metavar "SMELL"
          <> completeWith getSmellIds
          <> help "Select a smell to use, can be used multiple times to select multiple smells"
      ))
    <*> switch
      ( long "random-smells"
          <> help "Should random smells be used"
      )
    <*> switch
      ( long "inject"
          <> help "Inject the selected smells"
      )
    <*> switch
      ( long "fix"
          <> help "Fix the selected smells"
      )
    <*> optional (option
      auto
      ( long "seed"
          <> help "Seed for the random number generator used by the inectors and for the smell selection"
          <> metavar "INT"
      ))
    <*> optional (strOption
      ( long "modified-date"
          <> help "The latest modified date of the input Dockerfile (for fixing version pinning smells)"
      ))
