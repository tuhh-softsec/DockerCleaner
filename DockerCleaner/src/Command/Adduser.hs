{-# LANGUAGE NamedFieldPuns #-}

module Command.Adduser (Adduser (..), defaultAdduser) where

import Command (Command, parse, toAST)
import CommandParser (stringToWordToken, stringsToWordTokens)
import ShellCheck.AST (Id (Id), InnerToken (Inner_T_Literal, Inner_T_NormalWord, Inner_T_SimpleCommand), Token (OuterToken))

data Adduser = Adduser
  { name :: String,
    gid :: Maybe String,
    system :: Bool
  }
  deriving (Show)

defaultAdduser =
  Adduser
    { name = "",
      gid = Nothing,
      system = False
    }

addBoolFlag :: (Adduser -> Bool) -> String -> Adduser -> [Token]
addBoolFlag extractor flag command = if extractor command then stringsToWordTokens [flag] else []

addParamFlag :: (a -> String) -> (Adduser -> Maybe a) -> String -> Adduser -> [Token]
addParamFlag toString extractor flag command = case extractor command of
  Just i -> stringsToWordTokens [flag, toString i]
  _ -> []

addStringFlag :: (Adduser -> Maybe String) -> String -> Adduser -> [Token]
addStringFlag = addParamFlag id

addSytem = addBoolFlag system "-S"

addGid = addStringFlag gid "-G"


addFlags :: Adduser -> [Token]
addFlags useradd = concatMap (\f -> f useradd) [addSytem, addGid]

instance Command Adduser where
  parse _ = Nothing
  toAST useradd@Adduser {name} = Inner_T_SimpleCommand [] $ stringToWordToken "adduser" : addFlags useradd ++ [stringToWordToken name]
