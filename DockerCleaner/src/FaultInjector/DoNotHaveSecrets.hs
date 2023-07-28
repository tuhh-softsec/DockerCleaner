{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module FaultInjector.DoNotHaveSecrets where

import Control.Monad (liftM2)
import Data.Char (isAlphaNum, isAscii, isLower, isNumber, isPrint, isUpper)
import Data.Monoid (Any)
import Data.Text (Text, append, cons, pack)
import Data.Word (Word64)
import Helper
import Language.Docker (BaseImage (digest, tag), Instruction (Env, From), InstructionPos (instruction))
import RandomHelper (fromGen, vectorOfRange, (|++), (|++|), (++|))
import ShellCheck.AST (Token)
import System.Random (Random (randomR), RandomGen (split), StdGen, Uniform, mkStdGen, uniform, uniformR)
import System.Random.SplitMix (mkSMGen)
import System.Random.Stateful (Random (random), StatefulGen, Uniform (uniformM), UniformRange (uniformRM), runStateGen, splitGenM)
import Test.QuickCheck (Arbitrary (arbitrary), arbitraryPrintableChar, chooseInt, chooseInteger, suchThat, getPrintableString)
import Test.QuickCheck.Gen (Gen (MkGen, unGen), elements, oneof, vectorOf)
import Test.QuickCheck.Random (QCGen (QCGen))
import Dockerfile (Dockerfile)

data Secret = Secret {enviormentVariable :: Text, value :: Text}

isPasswordChar c = isPrint c && isAscii c

isBase64 :: (Char, Char) -> Char -> Bool
isBase64 (specialCharA, specialCharB) c = isAscii c && (isAlphaNum c || c == specialCharA || c == specialCharB)

isAlphaNumUpper c = isAscii c && (isNumber c || elem c ['A' .. 'Z'])

isAsciiAlphaNum c = isAscii c && isAlphaNum c

arbitraryPassword = getPrintableString <$> arbitrary

arbitraryNumber length = vectorOf length $ suchThat arbitrary isNumber

arbitraryBase64 speicalChars length = vectorOf length $ suchThat arbitrary $ isBase64 speicalChars

arbitraryBase16 length = vectorOf length $ suchThat arbitrary isAlphaNumUpper

arbitraryAsciiAlphaNum length = vectorOf length $ suchThat arbitrary isAsciiAlphaNum

arbitraryGoogleApiKey = "AIza" ++| arbitraryAsciiAlphaNum 35

arbitrarySlackToken = elements ["xoxb", "xoxp", "xapp"] |++ "-" |++| arbitraryNumber 12 |++ "-" |++| arbitraryBase64 ('/', '-') 24

arbitraryNpmToken = "npm_" ++| arbitraryBase64 ('/', '-') 36

arbitraryGithubToken = "ghp_" ++| arbitraryBase64 ('/', '-') 36

arbitraryAwsAccessKey = oneof [elements ["AKIA", "ASIA"], "A3T" ++| arbitraryBase16 1] |++| arbitraryBase16 16

arbitraryAwsSecretKey = arbitraryBase64 ('/', '-') 40

arbitraryConsumerSecret = arbitraryBase64 ('/', '-') 52

arbitrarySafePassword = arbitraryBase64 ('/', '-') 40

instance Arbitrary Secret where
  arbitrary =
    oneof
      [ Secret "AWS_ACCESS_KEY" . pack <$> arbitraryAwsAccessKey,
        Secret "AWS_SECRET_KEY" . pack <$> arbitraryAwsSecretKey,
        Secret "DOCKER_PASSWORD" . pack <$> arbitrarySafePassword,
        Secret "GITHUB_TOKEN" . pack <$> arbitraryGithubToken,
        Secret "GOOGLE_API_KEY" . pack <$> arbitraryGoogleApiKey,
        Secret "NPM_TOKEN" . pack <$> arbitraryNpmToken,
        Secret "SLACK_TOKEN" . pack <$> arbitrarySlackToken,
        Secret "POSTGRES_PASSWORD" . pack <$> arbitrarySafePassword,
        Secret "CONSUMER_SECRET" . pack <$> arbitraryConsumerSecret
      ]

arbitrarySecrets :: Gen [Secret]
arbitrarySecrets = vectorOfRange (1, 5) arbitrary

secretToPair :: Secret -> (Text, Text)
secretToPair Secret {enviormentVariable, value} = (enviormentVariable, value)

injectFault :: RandomGen g => g -> Dockerfile -> Dockerfile
injectFault randomGenerator instructions = take injectAfter instructions ++ instructionToInstructionPos (Env $ map secretToPair secrets) : drop injectAfter instructions
  where
    (randGen1, randGen2) = split randomGenerator
    amountOfInstructions = length instructions
    (secrets, _) = fromGen arbitrarySecrets randGen1
    injectAfter = length instructions
