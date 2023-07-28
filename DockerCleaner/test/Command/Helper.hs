{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Command.Helper where

import Command (Command, parse, toAST)
import Data.Text (pack)
import Data.Text.Array (Array (aBA))
import DockerfileParser (parseShell)
import PrintShellAST (printToken)
import ShellCheck.AST (Id (Id), InnerToken (Inner_T_Annotation, Inner_T_Pipeline, Inner_T_Redirecting, Inner_T_Script, Inner_T_SimpleCommand), Token (OuterToken))
import Test.QuickCheck (Property, (===))

pattern Command t <- OuterToken _ (Inner_T_Annotation _ (OuterToken _ (Inner_T_Script _ ((OuterToken _ (Inner_T_Pipeline _ ((OuterToken _ (Inner_T_Redirecting _ (OuterToken _ t))) : _))) : _))))

parseCommand :: Command a => String -> Maybe a
parseCommand command = case parseShell (pack command) of
  Just (Command t) -> parse t
  _ -> Nothing

parseCommandAndShow :: forall proxy a. Command a => Show a => proxy a -> String -> String
parseCommandAndShow _ = show . (parseCommand :: String -> Maybe a)

parseAndToAST :: forall proxy a. Command a => proxy a -> String -> Maybe String
parseAndToAST _ command = fmap (printToken . OuterToken (Id 0) . toAST) (parseCommand command :: Maybe a)

(?===) :: (Eq a, Show a) => Maybe a -> a -> Property
(?===) (Just a) b = a === b
(?===) Nothing b = Nothing === Just b

commandParseTestHelper :: Command a => String -> (a -> Bool) -> Bool
commandParseTestHelper command resultMatcher = case parseCommand command of
  Just result -> resultMatcher result
  _ -> False