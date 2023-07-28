{-# LANGUAGE TemplateHaskell #-}

module Command.AptGetSpec (runTests) where

import Command.AptGet
import Command.Helper
import Package
import Data.Either (Either (Right))
import Test.QuickCheck (quickCheckAll)

prop_parse_ex1 = commandParseTestHelper "apt-get install --no-install-recommends bash=1.0.2" matcher
  where
    matcher AptGetInstall {noInstallRecommends = True, packages = [Right Package {name = "bash", version = "1.0.2"}]} = True
    matcher _ = False

prop_parse_ex2 = commandParseTestHelper "apt-get install bash=1.0.2" matcher
  where
    matcher AptGetInstall {noInstallRecommends = False, packages = [Right Package {name = "bash", version = "1.0.2"}]} = True
    matcher _ = False

prop_parse_ex3 = commandParseTestHelper "apt-get install bash" matcher
  where
    matcher AptGetInstall {packages = [Right Package {name = "bash", version = ""}], unparsed = []} = True
    matcher _ = False

prop_parse_ex4 = commandParseTestHelper "apt-get install bash curl=43.1 jq=2.0.0" matcher
  where
    matcher AptGetInstall {packages = [Right Package {name = "bash", version = ""}, Right Package {name = "curl", version = "43.1"}, Right Package {name = "jq", version = "2.0.0"}]} = True
    matcher _ = False

prop_parse_ex5 = commandParseTestHelper "apt-get update" matcher
  where
    matcher AptGetUpdate {unparsed = []} = True
    matcher _ = False

prop_parse_ex6 = not (commandParseTestHelper "apt-get upgrade" (const True :: AptGet -> Bool))

-- QuickCheck test runner

return []

runTests = $quickCheckAll
