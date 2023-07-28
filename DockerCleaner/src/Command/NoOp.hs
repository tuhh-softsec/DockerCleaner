module Command.NoOp (NoOp (NoOp)) where

import Command (Command, parse, toAST)
import ShellCheck.AST (Id (Id), InnerToken (Inner_T_Literal, Inner_T_NormalWord, Inner_T_SimpleCommand), Token (OuterToken))

data NoOp = NoOp
  deriving (Show)

instance Command NoOp where
  parse (Inner_T_SimpleCommand [] [OuterToken _ (Inner_T_NormalWord [OuterToken (Id _) (Inner_T_Literal ":")])]) = Just NoOp
  parse _ = Nothing

  toAST NoOp = Inner_T_SimpleCommand [] [OuterToken (Id 0) (Inner_T_NormalWord [OuterToken (Id 1) (Inner_T_Literal ":")])]
