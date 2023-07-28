{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module ToJSON where

import Data.Aeson (ToJSON (toEncoding, toJSON), Value (String))
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy as L (unpack)
import GHC.Generics (Generic)
import Language.Docker
  ( AddArgs (AddArgs),
    Arguments,
    BaseImage (digest, tag),
    Check (Check, NoCheck),
    CheckArgs (CheckArgs, checkCommand),
    CopyArgs (CopyArgs, sourceFlag),
    CopySource (NoSource),
    Instruction (..),
    InstructionPos (InstructionPos, instruction),
    parseFile,
  )
import Language.Docker.Syntax
  ( Arguments (ArgumentsList, ArgumentsText),
    RunArgs (RunArgs),
  )
import ShellCheck.AST
import ShellCheck.Interface (ParseResult, mockedSystemInterface, newParseSpec, prRoot, psFilename, psScript)
import ShellCheck.Parser (parseScript)
import Text.Parsec (SourcePos)

deriving instance ToJSON Id

deriving instance Generic ConditionType

deriving instance ToJSON ConditionType

instance ToJSON SourcePos where
  toJSON x = String ""

deriving instance Generic AssignmentMode

deriving instance ToJSON AssignmentMode

deriving instance Generic CaseType

deriving instance ToJSON CaseType

deriving instance Generic FunctionKeyword

deriving instance ToJSON FunctionKeyword

deriving instance Generic FunctionParentheses

deriving instance ToJSON FunctionParentheses

deriving instance Generic Dashed

deriving instance ToJSON Dashed

deriving instance Generic Quoted

deriving instance ToJSON Quoted

deriving instance Generic Annotation

deriving instance ToJSON Annotation

deriving instance Generic a => Generic (InnerToken a)

deriving instance (ToJSON a, Generic a) => ToJSON (InnerToken a)

deriving instance Generic Token

instance ToJSON Token where
  toJSON (OuterToken _ innerToken) = toJSON innerToken
  toEncoding (OuterToken _ innerToken) = toEncoding innerToken

tokenToJSONString :: Token -> String
tokenToJSONString = L.unpack . encodeToLazyText
