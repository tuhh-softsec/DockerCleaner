{-# LANGUAGE NamedFieldPuns #-}

module Command.Groupadd (Groupadd (..), defaultGroupadd) where

import Command (Command, parse, toAST)
import CommandParser (stringsToWordTokens, stringToWordToken)
import ShellCheck.AST (Id (Id), InnerToken (Inner_T_Literal, Inner_T_NormalWord, Inner_T_SimpleCommand), Token (OuterToken))

data Groupadd = Groupadd
  { 
    group :: String,
    force :: Bool,
    gid :: Maybe Int,
    help :: Bool,
    key :: Maybe String,
    nonUnique :: Bool,
    password :: Maybe String,
    system :: Bool,
    chrootDir :: Maybe String,
    prefixDir :: Maybe String,
    extrausers :: Bool
  }
  deriving (Show)

defaultGroupadd = Groupadd {
  group = "",
  force = False,
  gid = Nothing,
  help = False,
  key = Nothing,
  nonUnique = False,
  password = Nothing,
  system = False,
  chrootDir = Nothing,
  prefixDir = Nothing,
  extrausers = False
}

addBoolFlag :: (Groupadd -> Bool) -> String -> Groupadd -> [Token]
addBoolFlag extractor flag command = if extractor command then stringsToWordTokens [flag] else []

addParamFlag :: (a -> String) -> (Groupadd -> Maybe a) -> String -> Groupadd -> [Token]
addParamFlag toString extractor flag command = case extractor command of
  Just i -> stringsToWordTokens [flag, toString i]
  _ -> []

addIntFlag :: (Groupadd -> Maybe Int) -> String -> Groupadd -> [Token]
addIntFlag = addParamFlag show

addStringFlag :: (Groupadd -> Maybe String) -> String -> Groupadd -> [Token]
addStringFlag = addParamFlag id

addForce = addBoolFlag force "--force"
addSytem = addBoolFlag system "--system"
addGid = addIntFlag gid "--gid"
addKey = addStringFlag key "--key"
addNonUnique = addBoolFlag nonUnique "--non-unique"
addPassword = addStringFlag password "--password"
addChrootDir = addStringFlag chrootDir "--root"
addPrefixDir = addStringFlag prefixDir "--prefix"
addExtrausers = addBoolFlag extrausers "--extrausers"
addHelp = addBoolFlag help "--help"

addFlags :: Groupadd -> [Token]
addFlags groupAdd = concatMap (\f -> f groupAdd) [addForce, addSytem, addGid, addKey, addNonUnique, addPassword, addChrootDir, addPrefixDir, addExtrausers, addHelp]

instance Command Groupadd where
  parse _ = Nothing
  toAST groupAdd@Groupadd {group} = Inner_T_SimpleCommand [] $ stringToWordToken "groupadd" : addFlags groupAdd ++ [stringToWordToken group]
