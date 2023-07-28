{-# LANGUAGE TemplateHaskell #-}

module Command.NpmSpec (runTests) where

import Command.Helper
import Command.Npm
import Package
import Data.Either (Either (Right))
import Test.QuickCheck (quickCheckAll)

prop_parse_ex1 = commandParseTestHelper "npm install eslint@1.2.3" matcher
  where
    matcher NpmInstall {packages = [Right Package {name = "eslint", version = "1.2.3"}]} = True
    matcher _ = False

prop_parse_ex2 = commandParseTestHelper "npm install --save-dev eslint@1.2.3 typescript@3.4.1" matcher
  where
    matcher NpmInstall {packages = [Right Package {name = "eslint", version = "1.2.3"}, Right Package {name = "typescript", version = "3.4.1"}]} = True
    matcher _ = False

prop_parse_ex3 = commandParseTestHelper "npm i -w dev-workspace typescript" matcher
  where
    matcher NpmInstall {packages = [Right Package {name = "typescript", version = ""}]} = True
    matcher _ = False

prop_parse_and_toAST_ex1 = parseAndToAST proxy "npm install --save-dev eslint@1.2.3 typescript@3.4.1" ?=== "npm install eslint@1.2.3 typescript@3.4.1 --save-dev"

prop_parse_and_toAST_ex2 = parseAndToAST proxy "npm i -w dev-workspace typescript" ?=== "npm install typescript -w dev-workspace"

-- QuickCheck test runner

return []

runTests = $quickCheckAll
