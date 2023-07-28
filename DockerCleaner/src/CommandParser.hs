{-# LANGUAGE FlexibleInstances #-}

module CommandParser where

import Data.Maybe (listToMaybe, catMaybes, isJust)
import PrintShellAST (printSimpleToken, printToken)
import ShellCheck.AST (Id (Id), InnerToken (..), Token (OuterToken))
import Control.Monad (join)
import Data.List (find)

class Tokenlike t where
  getText :: t -> String
  getSimpleText :: t -> Maybe String
  toInnerToken :: t -> InnerToken Token
  toToken :: t -> Token

instance Tokenlike Token where
  getText = printToken
  getSimpleText = printSimpleToken
  toInnerToken (OuterToken _ token) = token
  toToken = id

instance Tokenlike (InnerToken Token) where
  getText = printToken . toToken
  getSimpleText = printSimpleToken . toToken
  toInnerToken = id
  toToken = OuterToken (Id 0)

newtype ParsedCommand t = ParsedCommand [Parameter t]

data Parameter t = FlagWithParameter t t | Flag t | Parameter t

parseParameters :: Tokenlike t => (t -> Bool) -> [t] -> [Parameter t]
parseParameters isFlagWithParameter (flag : parameter : ts) | isFlagWithParameter flag = FlagWithParameter flag parameter : parseParameters isFlagWithParameter ts
parseParameters isFlagWithParameter (t : ts) | isFlagParameter t = Flag t : parseParameters isFlagWithParameter ts
  where
    isFlagParameter :: Tokenlike t => t -> Bool
    isFlagParameter t = case getSimpleText $ toInnerToken t of
      Just ('-' : _) -> True
      _ -> False
parseParameters isFlagWithParameter (t : ts) = Parameter t : parseParameters isFlagWithParameter ts
parseParameters _ [] = []

parseSubCommand :: (InnerToken Token -> Bool) -> (Token -> Bool) -> (Token -> Bool) -> InnerToken Token -> Maybe (ParsedCommand Token)
parseSubCommand isCommand isSubCommand isFlagWithParameter command@(Inner_T_SimpleCommand [] (_ : arguments))
  | isCommand command && maybe False isSubCommand subCommand = Just $ ParsedCommand $ parseParameters isFlagWithParameter parameters
    where
      subCommand = find (not . seemsLikeAFlag) arguments
      parameters = filter (not . isSubCommand) arguments
      seemsLikeAFlag t = (fmap head $ (getSimpleText . toInnerToken) t) == Just '-'
parseSubCommand _ _ _ _ = Nothing

parseCommand2 :: String -> [String] -> [String] -> InnerToken Token -> Maybe (ParsedCommand Token)
parseCommand2 command subCommandAliases flagsWithParameter = parseSubCommand (matchCommandName command) (\t -> any (`isParameter` t) subCommandAliases) (isFlagWithParameters flagsWithParameter)

parseCommand :: (InnerToken Token -> Bool) -> (Token -> Bool) -> InnerToken Token -> Maybe (ParsedCommand Token)
parseCommand isCommand isFlagWithParameter command@(Inner_T_SimpleCommand [] (_ : parameters))
  | isCommand command = Just $ ParsedCommand $ parseParameters isFlagWithParameter parameters
parseCommand _ _ _ = Nothing

parseCommand3 :: String -> [String] -> InnerToken Token -> Maybe (ParsedCommand Token)
parseCommand3 command flagsWithParameter = parseCommand (matchCommandName command) (isFlagWithParameters flagsWithParameter)

matchName :: Tokenlike t => String -> t -> Bool
matchName a t = (getSimpleText . toInnerToken) t == Just a

matchCommandName :: String -> InnerToken Token -> Bool
matchCommandName name (Inner_T_SimpleCommand [] ((OuterToken _ commandName) : _)) = matchName name commandName
matchCommandName _ _ = False

isFlag :: Tokenlike t => String -> t -> Bool
isFlag = matchName

isParameter :: Tokenlike t => String -> t -> Bool
isParameter = matchName

isFlagParameter :: Parameter a -> Bool
isFlagParameter (Flag _) = True
isFlagParameter _ = False

stringToWordToken :: String -> Token
stringToWordToken s = OuterToken (Id 0) $ Inner_T_NormalWord [OuterToken (Id 1) (Inner_T_Literal s)]

stringsToWordTokens :: [String] -> [Token]
stringsToWordTokens = map stringToWordToken

addWhen :: Bool -> a -> [a] -> [a]
addWhen True x xs = x : xs
addWhen False _ xs = xs

addAllWhen :: Bool -> [a] -> [a] -> [a]
addAllWhen True xs1 xs2 = xs1 ++ xs2
addAllWhen False _ xs = xs

isFlagWithParameters :: Tokenlike t => [String] -> t -> Bool
isFlagWithParameters flags token = any (`isFlag` token) flags
