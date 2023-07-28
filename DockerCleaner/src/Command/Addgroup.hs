{-# LANGUAGE NamedFieldPuns #-}

module Command.Addgroup (Addgroup (..), defaultAddgroup) where

import Command (Command, parse, toAST)
import CommandParser (stringsToWordTokens, stringToWordToken)
import ShellCheck.AST (Id (Id), InnerToken (Inner_T_Literal, Inner_T_NormalWord, Inner_T_SimpleCommand), Token (OuterToken))

data Addgroup = Addgroup
  { 
    group :: String,
    system :: Bool
  }
  deriving (Show)

defaultAddgroup = Addgroup {
  group = "",
  system = False
}

addBoolFlag :: (Addgroup -> Bool) -> String -> Addgroup -> [Token]
addBoolFlag extractor flag command = if extractor command then stringsToWordTokens [flag] else []

addSytem = addBoolFlag system "-S"

addFlags :: Addgroup -> [Token]
addFlags addGroup = concatMap (\f -> f addGroup) [addSytem]

instance Command Addgroup where
  parse _ = Nothing
  toAST addGroup@Addgroup {group} = Inner_T_SimpleCommand [] $ stringToWordToken "addgroup" : addFlags addGroup ++ [stringToWordToken group]
