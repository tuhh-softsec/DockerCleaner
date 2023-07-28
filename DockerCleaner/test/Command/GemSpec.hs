{-# LANGUAGE TemplateHaskell #-}

module Command.GemSpec (runTests) where

import Command.Gem
import Command.Helper
import Package
import Test.QuickCheck (quickCheckAll)

prop_parse_ex1 = commandParseTestHelper "gem install async-http -v 0.54.0" matcher
  where
    matcher GemInstall {package = Package {name = "async-http", version = "0.54.0"}, unparsed = []} = True
    matcher _ = False

prop_parse_and_toAST_ex1 = parseAndToAST proxy "gem install async-http -v 0.54.0" ?=== "gem install async-http --version 0.54.0"

prop_parse_and_toAST_ex2 = parseAndToAST proxy "gem install async-http --version 0.54.0" ?=== "gem install async-http --version 0.54.0"

prop_parse_and_toAST_ex3 = parseAndToAST proxy "gem i --user-install async-http" ?=== "gem install async-http --user-install"

-- QuickCheck test runner

return []

runTests = $quickCheckAll
