{-# LANGUAGE TemplateHaskell #-}

module Command.PipSpec (runTests) where

import Command.Helper
import Command.Pip
import Package
import Test.QuickCheck (quickCheckAll)
import ShellCheck.AST (Token(OuterToken))
import Data.Either (Either(Right, Left))

prop_parse_ex1 = commandParseTestHelper "pip install --no-cache-dir pip==1.0.0" matcher
  where
    matcher PipInstall {packages = [Right Package {name = "pip", version = "1.0.0"}]} = True
    matcher _ = False

prop_parse_ex2 = commandParseTestHelper "pip install --no-cache-dir pip==1.0.0 wheel==2.1.3" matcher
  where
    matcher PipInstall {packages = [Right Package {name = "pip", version = "1.0.0"}, Right Package {name = "wheel", version = "2.1.3"}]} = True
    matcher _ = False

prop_parse_ex3 = commandParseTestHelper "pip install pip==1.0.0 --no-cache-dir wheel==2.1.3" matcher
  where
    matcher PipInstall {packages = [Right Package {name = "pip", version = "1.0.0"}, Right Package {name = "wheel", version = "2.1.3"}]} = True
    matcher _ = False

prop_parse_ex4 = commandParseTestHelper "pip install -b test --no-cache-dir pip==1.0.0 wheel==2.1.3" matcher
  where
    matcher PipInstall {packages = [Right Package {name = "pip", version = "1.0.0"}, Right Package {name = "wheel", version = "2.1.3"}]} = True
    matcher _ = False

prop_parse_ex5 = commandParseTestHelper "pip install --no-cache-dir pip==$PIP setuptools==$SETUPTOOLS zc.buildout==2.0.0 wheel==$WHEEL" matcher
  where
    matcher PipInstall {packages = [Left OuterToken {}, Left OuterToken {}, Right Package {name = "zc.buildout", version = "2.0.0"}, Left OuterToken {}]} = True
    matcher _ = False

prop_parse_and_toAST_ex1 = parseAndToAST proxy "pip install pip==1.0.0 --no-cache-dir wheel==2.1.3" ?=== "pip install pip==1.0.0 wheel==2.1.3 --no-cache-dir"

prop_parse_and_toAST_ex2 = parseAndToAST proxy "pip install -b test --no-cache-dir pip==1.0.0 wheel==2.1.3" ?=== "pip install pip==1.0.0 wheel==2.1.3 -b test --no-cache-dir"

prop_parse_and_toAST_ex3 = parseAndToAST proxy "pip install pip==1.0.0" ?=== "pip install pip==1.0.0"

prop_parse_and_toAST_ex4 = parseAndToAST proxy "pip install pip" ?=== "pip install pip"

prop_parse_and_toAST_ex5 = parseAndToAST proxy "pip install --no-cache-dir pip==$PIP setuptools==$SETUPTOOLS zc.buildout==2.0.0 wheel==$WHEEL" ?=== "pip install zc.buildout==2.0.0 pip==$PIP setuptools==$SETUPTOOLS wheel==$WHEEL --no-cache-dir"

-- QuickCheck test runner

return []

runTests = $quickCheckAll
