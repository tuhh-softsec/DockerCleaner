{-# LANGUAGE TemplateHaskell #-}

module Command.WgetSpec (runTests) where

import Command.Helper
import Command.Wget
import Package
import Test.QuickCheck (quickCheckAll)

prop_parse_ex1 = commandParseTestHelper "wget http://example.com/folder/file" matcher
  where
    matcher Wget {url = Just "http://example.com/folder/file"} = True
    matcher _ = False

prop_parse_ex2 = commandParseTestHelper "wget -r -l 3 -np -p --user-agent=\"Mozilla/5.0 (X11; U; Linux i686; de; rv:1.9b5) Gecko/2008050509 Firefox/3.0b5\" http://example.com/dir/" matcher
  where
    matcher Wget {url = Just "http://example.com/dir/"} = True
    matcher _ = False

prop_parse_ex3 = commandParseTestHelper "wget -nv -O /usr/local/bin/gosu \"https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch\"" matcher
  where
    matcher Wget {url = Nothing, outputDocument = Just "/usr/local/bin/gosu"} = True
    matcher _ = False

prop_parse_and_toAST_ex1 = parseAndToAST proxy "wget http://example.com/folder/file" ?=== "wget http://example.com/folder/file"

prop_parse_and_toAST_ex2 = parseAndToAST proxy "wget -nv http://example.com/folder/file" ?=== "wget --no-verbose http://example.com/folder/file"

prop_parse_and_toAST_ex3 = parseAndToAST proxy "wget -r -l 3 -np -p --user-agent=\"Mozilla/5.0 (X11; U; Linux i686; de; rv:1.9b5) Gecko/2008050509 Firefox/3.0b5\" http://example.com/dir/" ?=== "wget -r -l 3 -np -p --user-agent=\"Mozilla/5.0 (X11; U; Linux i686; de; rv:1.9b5) Gecko/2008050509 Firefox/3.0b5\" http://example.com/dir/"

prop_parse_and_toAST_ex4 = parseAndToAST proxy "wget -nv -O /usr/local/bin/gosu \"https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch\"" ?=== "wget --no-verbose --output-document /usr/local/bin/gosu \"https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch\""

-- QuickCheck test runner

return []

runTests = $quickCheckAll
