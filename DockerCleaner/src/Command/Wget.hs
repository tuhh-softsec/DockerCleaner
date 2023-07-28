{-# LANGUAGE NamedFieldPuns #-}

module Command.Wget (Wget (Wget, url, unparsed, outputDocument, noVerbose), proxy) where

import Command (Command, InstallCommand (getPackages), mapPackages, parse, toAST)
import CommandParser (Parameter (Flag, FlagWithParameter, Parameter), ParsedCommand (ParsedCommand), Tokenlike (getSimpleText), addAllWhen, addWhen, isFlag, isFlagParameter, parseCommand3, stringsToWordTokens)
import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Aeson (Value (Bool))
import Data.List (isPrefixOf)
import Data.Maybe (fromJust, isJust, listToMaybe, mapMaybe, maybeToList)
import Data.Proxy (Proxy (Proxy))
import Package (Package (Package, name, version), toPackage)
import ShellCheck.AST (InnerToken (Inner_T_SimpleCommand), Token)
import System.Process (readCreateProcess, shell)

proxy :: Proxy Wget
proxy = Proxy

data Wget = Wget
  { url :: Maybe String,
    outputDocument :: Maybe String,
    noVerbose :: Bool,
    unparsed :: [Token]
  }
  deriving (Show)

instance Command Wget where
  parse = parseWget

  toAST Wget {url, noVerbose, outputDocument, unparsed} = Inner_T_SimpleCommand [] (stringsToWordTokens command ++ unparsed ++ stringsToWordTokens (maybeToList url))
    where
      command :: [String]
      command = "wget" : addWhen noVerbose "--no-verbose" (addAllWhen (isJust outputDocument) ["--directory-prefix", fromJust outputDocument] [])

parseWget :: InnerToken Token -> Maybe Wget
parseWget t = commandToWget <$> parseCommand3 "wget" ["-e", "--execute", "-l", "-o", "-a", "-i", "-P", "--directory-prefix"] t
  where
    commandToWget (ParsedCommand ts) =
      Wget
        { url = findUrl ts,
          outputDocument = findOutputDocument ts,
          noVerbose = isNoVerbose ts,
          unparsed = unparsed isParsed ts
        }
      where
        isOutputDocumentFlag t = isFlag "-P" t || isFlag "--directory-prefix" t
        findOutputDocument :: [Parameter Token] -> Maybe String
        findOutputDocument ts = join $ listToMaybe [getSimpleText param | FlagWithParameter flag param <- ts, isOutputDocumentFlag flag]
        findUrl :: [Parameter Token] -> Maybe String
        findUrl ts = join $ listToMaybe [getSimpleText t | Parameter t <- ts]
        isNoVerboseFlag :: Token -> Bool
        isNoVerboseFlag t = isFlag "-nv" t || isFlag "--no-verbose" t
        isNoVerbose :: [Parameter Token] -> Bool
        isNoVerbose ts = or [isNoVerboseFlag t | Flag t <- ts]
        isParsed :: Parameter Token -> Bool
        isParsed (Flag t) = isNoVerboseFlag t
        isParsed (FlagWithParameter flag _) = isOutputDocumentFlag flag
        isParsed (Parameter t) = isJust $ getSimpleText t
        unparsed :: (Parameter Token -> Bool) -> [Parameter Token] -> [Token]
        unparsed isParsed ts = parse =<< filter (not . isParsed) ts
          where
            parse :: Parameter Token -> [Token]
            parse (Parameter t) = [t]
            parse (FlagWithParameter flag param) = [flag, param]
            parse (Flag t) = [t]