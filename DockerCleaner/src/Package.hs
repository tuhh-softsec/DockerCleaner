module Package where

import CommandParser (Parameter (Parameter), Tokenlike (getSimpleText), getText)
import ShellCheck.AST (Token)

data Package = Package
  { name :: String,
    version :: String
  }
  deriving (Show)

toPackage :: Tokenlike t => (String -> String) -> (String -> String) -> t -> Either t Package
toPackage getName getVersion t = case getSimpleText t of
  Just s -> Right $ createPackage s
  _ -> Left t
  where
    createPackage text = Package {name = getName text, version = getVersion text}

toPackages :: Tokenlike t => (String -> String) -> (String -> String) -> [Parameter t] -> [Either t Package]
toPackages getName getVersion (Parameter t : xs) = toPackage getName getVersion t : toPackages getName getVersion xs
toPackages getName getVersion (_ : xs) = toPackages getName getVersion xs
toPackages _ _ [] = []
