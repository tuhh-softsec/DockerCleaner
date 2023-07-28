{-# LANGUAGE TemplateHaskell #-}

module Command.ApkSpec (runTests) where

import Command.Helper
import Command.Apk
import Package
import Data.Either (Either (Right))
import Test.QuickCheck (quickCheckAll)

prop_parse_ex1 = commandParseTestHelper "apk add wget" matcher
  where
    matcher ApkAdd {packages = [Right Package {name = "wget", version = ""}]} = True
    matcher _ = False

prop_parse_ex2 = commandParseTestHelper "apk add wget=1.2.3 curl=3.4.1" matcher
  where
    matcher ApkAdd {packages = [Right Package {name = "wget", version = "1.2.3"}, Right Package {name = "curl", version = "3.4.1"}]} = True
    matcher _ = False

prop_parse_ex3 = commandParseTestHelper "apk add --cache-dir /tmp -q wget=1.2.3 curl=3.4.1 --keys-dir /keys" matcher
  where
    matcher ApkAdd {packages = [Right Package {name = "wget", version = "1.2.3"}, Right Package {name = "curl", version = "3.4.1"}]} = True
    matcher _ = False

prop_parse_and_toAST_ex1 = parseAndToAST proxy "apk add --cache-dir /tmp -q wget=1.2.3 curl=3.4.1  --keys-dir /keys" ?=== "apk add wget=1.2.3 curl=3.4.1 --cache-dir /tmp -q --keys-dir /keys"

-- QuickCheck test runner

return []

runTests = $quickCheckAll
