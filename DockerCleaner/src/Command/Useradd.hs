{-# LANGUAGE NamedFieldPuns #-}

module Command.Useradd (Useradd (..), defaultUseradd) where

import Command (Command, parse, toAST)
import CommandParser (stringToWordToken, stringsToWordTokens)
import ShellCheck.AST (Id (Id), InnerToken (Inner_T_Literal, Inner_T_NormalWord, Inner_T_SimpleCommand), Token (OuterToken))

data Useradd = Useradd
  { name :: String,
    gid :: Maybe String,
    help :: Bool,
    key :: Maybe String,
    nonUnique :: Bool,
    noLogInit :: Bool,
    password :: Maybe String,
    system :: Bool,
    chrootDir :: Maybe String,
    prefixDir :: Maybe String,
    extrausers :: Bool
  }
  deriving (Show)

defaultUseradd =
  Useradd
    { name = "",
      gid = Nothing,
      help = False,
      key = Nothing,
      nonUnique = False,
      noLogInit = False,
      password = Nothing,
      system = False,
      chrootDir = Nothing,
      prefixDir = Nothing,
      extrausers = False
    }

addBoolFlag :: (Useradd -> Bool) -> String -> Useradd -> [Token]
addBoolFlag extractor flag command = if extractor command then stringsToWordTokens [flag] else []

addParamFlag :: (a -> String) -> (Useradd -> Maybe a) -> String -> Useradd -> [Token]
addParamFlag toString extractor flag command = case extractor command of
  Just i -> stringsToWordTokens [flag, toString i]
  _ -> []

addIntFlag :: (Useradd -> Maybe Int) -> String -> Useradd -> [Token]
addIntFlag = addParamFlag show

addStringFlag :: (Useradd -> Maybe String) -> String -> Useradd -> [Token]
addStringFlag = addParamFlag id

addSytem = addBoolFlag system "--system"

addGid = addStringFlag gid "--gid"

addKey = addStringFlag key "--key"

addNonUnique = addBoolFlag nonUnique "--non-unique"
addNoLogInit = addBoolFlag noLogInit "--no-log-init"

addPassword = addStringFlag password "--password"

addChrootDir = addStringFlag chrootDir "--root"

addPrefixDir = addStringFlag prefixDir "--prefix"

addExtrausers = addBoolFlag extrausers "--extrausers"

addHelp = addBoolFlag help "--help"

addFlags :: Useradd -> [Token]
addFlags useradd = concatMap (\f -> f useradd) [addSytem, addGid, addKey, addNonUnique, addNoLogInit, addPassword, addChrootDir, addPrefixDir, addExtrausers, addHelp]

instance Command Useradd where
  parse _ = Nothing
  toAST useradd@Useradd {name} = Inner_T_SimpleCommand [] $ stringToWordToken "useradd" : addFlags useradd ++ [stringToWordToken name]
