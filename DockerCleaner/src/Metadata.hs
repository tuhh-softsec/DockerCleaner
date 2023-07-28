{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Metadata where

import Control.Monad (ap, liftM)
import Data.Aeson (ToJSON)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Language.Docker (InstructionPos)
import ShellCheck.AST (Token)
import Smells (Smell (fixer, injector, smellId))
import System.Random (RandomGen (split), StdGen)
import Dockerfile (Dockerfile)

data Metadata = Metadata
  { fileName :: Maybe String,
    originalDockerfile :: Maybe String,
    originalDockerfileHash :: Maybe String,
    originalDockerfileUglified :: Maybe String,
    originalDockerfileUglifiedHash :: Maybe String,
    processedDockerfile :: Maybe String,
    processedDockerfileHash :: Maybe String,
    seed :: Maybe Int,
    injectedSmells :: [String],
    successfullyInjectedSmells :: [String],
    fixedSmells :: [String],
    successfullyFixedSmells :: [String]
  }

deriving instance Generic Metadata

deriving instance ToJSON Metadata

type DockerfileWithMetadata = (Dockerfile, Metadata)

newMetadata :: Metadata
newMetadata =
  Metadata
    { fileName = Nothing,
      originalDockerfile = Nothing,
      originalDockerfileHash = Nothing,
      originalDockerfileUglified = Nothing,
      originalDockerfileUglifiedHash = Nothing,
      processedDockerfile = Nothing,
      processedDockerfileHash = Nothing,
      seed = Nothing,
      injectedSmells = [],
      successfullyInjectedSmells = [],
      fixedSmells = [],
      successfullyFixedSmells = []
    }

injectWithMetadata :: Smell -> StdGen -> (Dockerfile, Metadata) -> (Dockerfile, Metadata)
injectWithMetadata smell gen (dockerfile, metadata@Metadata {injectedSmells, successfullyInjectedSmells}) = (newDockerfile, newMetadata)
  where
    newDockerfile = fromMaybe (const id) (injector smell) gen dockerfile
    newMetadata =
      metadata
        { injectedSmells = smellId smell : injectedSmells,
          successfullyInjectedSmells = if newDockerfile == dockerfile then successfullyInjectedSmells else smellId smell : successfullyInjectedSmells
        }

fixWithMetadata :: Smell -> (Dockerfile, Metadata) -> (Dockerfile, Metadata)
fixWithMetadata smell (dockerfile, metadata@Metadata {fixedSmells, successfullyFixedSmells}) = (newDockerfile, newMetadata)
  where
    newDockerfile = fromMaybe id (fixer smell) dockerfile
    newMetadata =
      metadata
        { fixedSmells = smellId smell : fixedSmells,
          successfullyFixedSmells = if newDockerfile == dockerfile then successfullyFixedSmells else smellId smell : successfullyFixedSmells
        }

injectAllWithMetadata :: [Smell] -> StdGen -> (Dockerfile, Metadata) -> (Dockerfile, Metadata)
injectAllWithMetadata smells generator (dockerfile, metadata) = fst $ foldr helper ((dockerfile, metadata), generator) smells
  where
    helper smell ((dockerfile, metadata), generator) = (injectWithMetadata smell g1 (dockerfile, metadata), g2)
      where
        (g1, g2) = split generator

fixAllWithMetadata :: [Smell] -> (Dockerfile, Metadata) -> (Dockerfile, Metadata)
fixAllWithMetadata smells (dockerfile, metadata) = foldr fixWithMetadata (dockerfile, metadata) smells
