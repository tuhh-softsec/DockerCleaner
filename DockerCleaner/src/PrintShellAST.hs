{-# LANGUAGE FlexibleInstances #-}

module PrintShellAST where

import Control.Monad (join)
import Data.Aeson (Value (String))
import Data.Functor.Identity (Identity (..))
import Data.List (intercalate)
import GHC.Generics (Generic1 (to1))
import ShellCheck.AST (Annotation, AssignmentMode (..), CaseType (..), ConditionType (..), Dashed (Undashed), FunctionKeyword (..), FunctionParentheses (..), Id (..), InnerToken (..), Quoted (Unquoted), Token (..))
import ShellCheck.Interface (ParseResult, mockedSystemInterface, newParseSpec, prRoot, psFilename, psScript)
import ShellCheck.Parser (parseScript)

class Printable a where
  print :: a -> String

instance Printable String where
  print a = a

instance Printable Token where
  print (OuterToken _ innerToken) = printInnerToken innerToken

(+++) :: (Printable a, Printable b) => a -> b -> String
(+++) a b = PrintShellAST.print a ++ PrintShellAST.print b

printToken :: Token -> String
printToken = PrintShellAST.print

printTokens :: String -> [Token] -> String
printTokens _ [] = ""
printTokens _ [t] = PrintShellAST.print t
printTokens joiner (t : ts) = PrintShellAST.print t +++ joiner +++ printTokens joiner ts

joinedWith :: [Token] -> String -> String
joinedWith = flip printTokens

isEmpty :: Token -> Bool
isEmpty (OuterToken _ (Inner_T_Literal "")) = True
isEmpty _ = False

isSequence :: Token -> Bool
isSequence (OuterToken _ Inner_TA_Sequence {}) = True
isSequence _ = False

printWithConditionType :: Printable a => ConditionType -> a -> String
printWithConditionType SingleBracket s = "[ " +++ s +++ " ]"
printWithConditionType DoubleBracket s = "[[ " +++ s +++ " ]]"

printSimpleToken :: Token -> Maybe String
printSimpleToken (OuterToken _ (Inner_T_NormalWord ts)) = intercalate "" <$> mapM printSimpleToken ts
printSimpleToken (OuterToken _ (Inner_T_Literal s)) = Just $ foldr replace "" s
  where
    replace '"' acc = '\\' : '"' : acc
    replace '`' acc = '\\' : '`' : acc
    replace c acc = c : acc
printSimpleToken (OuterToken _ (Inner_T_Pipeline [] [t])) = printSimpleToken t
printSimpleToken (OuterToken _ (Inner_T_Redirecting [] t)) = printSimpleToken t
printSimpleToken _ = Nothing

printInnerToken :: InnerToken Token -> String
printInnerToken (Inner_TA_Binary s t1 t2) = braceSequence t1 +++ " " +++ s +++ " " +++ braceSequence t2
  where
    braceSequence :: Token -> String
    braceSequence t | isSequence t = "(" +++ t +++ ")"
    braceSequence t = "" +++ t
printInnerToken (Inner_TA_Assignment s t1 t2) = t1 +++ " " +++ s +++ " " +++ t2
printInnerToken (Inner_TA_Variable s []) = s
printInnerToken (Inner_TA_Variable s ts) = s +++ "[" +++ (ts `joinedWith` "][") +++ "]"
printInnerToken (Inner_TA_Expansion ts) = ts `joinedWith` ""
printInnerToken (Inner_TA_Sequence ts) = ts `joinedWith` ", "
printInnerToken (Inner_TA_Trinary t1 t2 t3) = t1 +++ " ? " +++ t2 +++ " : " +++ t3
printInnerToken (Inner_TA_Unary ('|' : s) t) = t +++ s
printInnerToken (Inner_TA_Unary s t) | last s == '|' = init s +++ t
printInnerToken (Inner_TC_And conditionType s t1 t2) = t1 +++ " " +++ s +++ " " +++ t2
printInnerToken (Inner_TC_Binary conditionType s t1 t2) = t1 +++ " " +++ s +++ " " +++ t2
printInnerToken (Inner_TC_Group SingleBracket t) = "\\\\( " +++ t +++ " \\\\)"
printInnerToken (Inner_TC_Group DoubleBracket t) = "( " +++ t +++ " )"
printInnerToken (Inner_TC_Nullary conditionType t) = "" +++ t
printInnerToken (Inner_TC_Or conditionType s t1 t2) = t1 +++ " " +++ s +++ " " +++ t2
printInnerToken (Inner_TC_Unary conditionType s t) = s +++ " " +++ t
printInnerToken (Inner_TC_Empty conditionType) = ""
printInnerToken Inner_T_AND_IF = "&&"
printInnerToken (Inner_T_AndIf t1 t2) = t1 +++ " && " +++ t2
printInnerToken (Inner_T_Arithmetic t) = "((" +++ t +++ "))"
printInnerToken (Inner_T_Array ts) = "( " +++ (ts `joinedWith` " ") +++ " )"
printInnerToken (Inner_T_IndexedElement ts t) = t +++ "=" +++ (ts `joinedWith` " ")
printInnerToken (Inner_T_UnparsedIndex _ s) = "[" +++ s +++ "]"
printInnerToken (Inner_T_Assignment mode variable indices value) | all isEmpty indices = variable +++ operator +++ value
  where
    operator = case mode of
      Assign -> "="
      Append -> "+="
printInnerToken (Inner_T_Assignment mode variable indices value) = variable +++ "[" +++ (indices `joinedWith` "") +++ "]" +++ operator +++ value
  where
    operator = case mode of
      Assign -> "="
      Append -> "+="
printInnerToken (Inner_T_Backgrounded t) = t +++ "&"
printInnerToken (Inner_T_Backticked ts) = "`" +++ escape (ts `joinedWith` "") +++ "`"
  where
    escape :: String -> String
    escape ('`' : xs) = '\\' : '`' : escape xs
    escape ('\\' : xs) = '\\' : '\\' : escape xs
    escape (x : xs) = x : escape xs
    escape [] = ""
printInnerToken Inner_T_Bang = "! "
printInnerToken (Inner_T_Banged t) = "! " +++ t
printInnerToken (Inner_T_BraceExpansion ts) = "{" +++ (ts `joinedWith` ",") +++ "}"
printInnerToken (Inner_T_BraceGroup []) = "{ true; }"
printInnerToken (Inner_T_BraceGroup ts) = "{ " +++ (ts `joinedWith` ";") +++ "; }"
printInnerToken Inner_T_CLOBBER = ">|"
printInnerToken Inner_T_Case = "case"
printInnerToken (Inner_T_CaseExpression t cases) = "case " +++ t +++ " in " +++ concatMap printCase cases +++ " esac"
  where
    printCase :: (CaseType, [Token], [Token]) -> String
    printCase (caseType, ts1, ts2) = "(" +++ (ts1 `joinedWith` "|") +++ ") " +++ (ts2 `joinedWith` "; ") +++ separator
      where
        separator = case caseType of
          CaseContinue -> ";;&"
          CaseFallThrough -> ";&"
          CaseBreak -> ";;"
printInnerToken (Inner_T_Condition conditionType t) = printWithConditionType conditionType t
printInnerToken Inner_T_DGREAT = ">>"
printInnerToken Inner_T_DLESS = "<<"
printInnerToken Inner_T_DLESSDASH = "<<-"
printInnerToken Inner_T_DSEMI = ";;"
printInnerToken Inner_T_Do = "do"
printInnerToken (Inner_T_DollarArithmetic t) = "$((" +++ t +++ "))"
printInnerToken (Inner_T_DollarBraced True t) = "${" +++ t +++ "}"
printInnerToken (Inner_T_DollarBraced False t) = "$" +++ t
printInnerToken (Inner_T_DollarBracket t) = "$[" +++ t +++ "]"
printInnerToken (Inner_T_DollarDoubleQuoted ts) = "$\"" +++ (ts `joinedWith` "") +++ "\""
printInnerToken (Inner_T_DollarExpansion ts) = "$( " +++ (ts `joinedWith` ";") +++ ";)"
printInnerToken (Inner_T_DollarSingleQuoted s) = "$'" +++ s +++ "'"
printInnerToken (Inner_T_DollarBraceCommandExpansion ts) = "${ " +++ (ts `joinedWith` ";") +++ ";}"
printInnerToken Inner_T_Done = "done"
printInnerToken (Inner_T_DoubleQuoted ts) = "\"" +++ (ts `joinedWith` "") +++ "\""
printInnerToken Inner_T_EOF = ""
printInnerToken Inner_T_Elif = "elif"
printInnerToken Inner_T_Else = "else"
printInnerToken Inner_T_Esac = "esac"
printInnerToken (Inner_T_Extglob s ts) = s +++ "(" +++ (ts `joinedWith` "|") +++ ")"
printInnerToken (Inner_T_FdRedirect s t) = s +++ t
printInnerToken Inner_T_Fi = "fi"
printInnerToken Inner_T_For = "for"
printInnerToken (Inner_T_ForArithmetic x y z group) = "for ((" +++ x +++ ";" +++ y +++ ";" +++ z +++ ")); do " +++ (group `joinedWith` ";") +++ "; done"
printInnerToken (Inner_T_ForIn name values group) = "for " +++ name +++ " in " +++ (values `joinedWith` " ") +++ "; do " +++ (group `joinedWith` ";") +++ "; done"
printInnerToken (Inner_T_Function (FunctionKeyword True) (FunctionParentheses True) name group) = "function " +++ name +++ "() " +++ group
printInnerToken (Inner_T_Function (FunctionKeyword True) (FunctionParentheses False) name group) = "function " +++ name +++ " " +++ group
printInnerToken (Inner_T_Function (FunctionKeyword False) (FunctionParentheses True) name group) = name +++ "() " +++ group
printInnerToken Inner_T_GREATAND = ">&"
printInnerToken (Inner_T_Glob s) = s
printInnerToken Inner_T_Greater = ">"
printInnerToken (Inner_T_HereDoc dashed quoted endToken ts) = ">>> HEREDOC IS NOT CURRENTLY SUPPORTED <<<"
printInnerToken (Inner_T_HereString t) = "<<< " +++ t
printInnerToken Inner_T_If = "if"
printInnerToken (Inner_T_IfExpression (ifPart : elifParts) elsePart) = printIfPart ifPart +++ concatMap printElifPart elifParts +++ printElsePart elsePart +++ "fi"
  where
    printIfPart :: ([Token], [Token]) -> String
    printIfPart (condition, actions) = "if " +++ (condition `joinedWith` "") +++ "; then " +++ (actions `joinedWith` ";") +++ "; "
    printElifPart :: ([Token], [Token]) -> String
    printElifPart (condition, actions) = "elif " +++ (condition `joinedWith` "") +++ "; then " +++ (actions `joinedWith` ";") +++ "; "
    printElsePart :: [Token] -> String
    printElsePart [] = ""
    printElsePart actions = "else \n" +++ (actions `joinedWith` ";") +++ "; "
printInnerToken Inner_T_In = "in"
printInnerToken (Inner_T_IoFile op file) = op +++ " " +++ file
printInnerToken (Inner_T_IoDuplicate op target) = op +++ target
printInnerToken Inner_T_LESSAND = "<&"
printInnerToken Inner_T_LESSGREAT = "<>"
printInnerToken Inner_T_Lbrace = "{"
printInnerToken Inner_T_Less = "<"
printInnerToken (Inner_T_Literal s) = foldr replace "" s
  where
    replace '"' acc = '\\' : '"' : acc
    replace '`' acc = '\\' : '`' : acc
    replace c acc = c : acc
printInnerToken Inner_T_Lparen = "("
printInnerToken Inner_T_NEWLINE = "\n"
printInnerToken (Inner_T_NormalWord ts) = ts `joinedWith` ""
printInnerToken Inner_T_OR_IF = "||"
printInnerToken (Inner_T_OrIf t1 t2) = t1 +++ " || " +++ t2
printInnerToken (Inner_T_ParamSubSpecialChar s) = s
printInnerToken (Inner_T_Pipeline [] [t]) = "" +++ t
printInnerToken (Inner_T_Pipeline (pipeSeparator : pipeSepreators) (t : ts)) | not (null ts) = removeLastSemicolon ("" +++ t) +++ " " +++ pipeSeparator +++ " " +++ printInnerToken (Inner_T_Pipeline pipeSepreators ts)
  where
    removeLastSemicolon :: String -> String
    removeLastSemicolon = reverse . helper . reverse
      where
        helper :: String -> String
        helper (' ' : xs) = helper xs
        helper (';' : xs) = xs
        helper xs = xs
printInnerToken (Inner_T_ProcSub dir list) = dir +++ "(" +++ (list `joinedWith` "") +++ ")"
printInnerToken Inner_T_Rbrace = "}"
printInnerToken (Inner_T_Redirecting ts t) = t +++ " " +++ (ts `joinedWith` " ")
printInnerToken Inner_T_Rparen = ")"
printInnerToken (Inner_T_Script shebang commands) | isEmpty shebang = commands `joinedWith` "; "
printInnerToken (Inner_T_Script shebang commands) = "#!" +++ shebang +++ "\n" +++ (commands `joinedWith` "; ")
printInnerToken Inner_T_Semi = ";"
printInnerToken (Inner_T_SimpleCommand [] ts2) = ts2 `joinedWith` " "
printInnerToken (Inner_T_SimpleCommand ts1 ts2) = ts1 `joinedWith` " " +++ " " +++ printInnerToken (Inner_T_SimpleCommand [] ts2)
printInnerToken (Inner_T_SingleQuoted s) = "'" +++ s +++ "'"
printInnerToken (Inner_T_Subshell ts) = "(" +++ (ts `joinedWith` ";") +++ ")"
printInnerToken Inner_T_Then = "then"
printInnerToken Inner_T_Until = "until"
printInnerToken (Inner_T_UntilExpression condition statements) = "until " +++ (condition `joinedWith` " ") +++ "; do " +++ (statements `joinedWith` ";") +++ "; done"
printInnerToken Inner_T_While = "while"
printInnerToken (Inner_T_WhileExpression condition statements) = "while " +++ (condition `joinedWith` " ") +++ "; do " +++ (statements `joinedWith` ";") +++ "; done"
printInnerToken (Inner_T_Annotation _ t) = "" +++ t
printInnerToken (Inner_T_Pipe s) = s
printInnerToken (Inner_T_CoProc (Just s) t) = "coproc " +++ s +++ t
printInnerToken (Inner_T_CoProc Nothing t) = "coproc " +++ t
printInnerToken (Inner_T_CoProcBody t) = "" +++ t
printInnerToken (Inner_T_Include t) = "" +++ t
printInnerToken (Inner_T_SourceCommand t1 t2) = ">>> SOURCECOMMAND IS NOT CURRENTLY SUPPORTED <<<"
printInnerToken (Inner_T_BatsTest name test) = "@test " +++ name +++ " " +++ test
printInnerToken t = ">>> SOMETHING IS NOT CURRENTLY SUPPORTED <<<"

printShellText :: (Token -> String) -> Maybe Token -> String
printShellText f (Just a) = f a
printShellText _ Nothing = ":("

parseShell :: String -> Maybe Token
parseShell = prRoot . unpackIdentity . parse
  where
    parse s = parseScript (mockedSystemInterface []) newParseSpec {psScript = s}
    unpackIdentity (Identity a) = a
