{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper where

import Command (Command (toAST), InstallCommand, fmapCommand)
import Data.Functor.Identity (Identity (..))
import Data.List (nub, foldl')
import Data.Text (Text, pack, unpack)
import Language.Docker
  ( Arguments,
    Check (Check, NoCheck),
    CopySource (NoSource),
    Instruction (..),
    InstructionPos (InstructionPos, instruction),
    parseFile,
    parseText,
  )
import Language.Docker.Parser (Error)
import Language.Docker.Syntax
import ShellCheck.AST (Id (..), InnerToken (..), Token (OuterToken))
import ShellCheck.Interface (mockedSystemInterface, newParseSpec, prRoot, psFilename, psScript)
import ShellCheck.Parser (parseScript)
import Data.Word (Word8, Word32)
import Crypto.Hash (Digest, MD5, hash)
import qualified Data.ByteArray as BA
import Data.Text.Encoding (encodeUtf16LE)
import Data.Bits ((.|.), shiftL)
import Data.Set (empty)

deriving instance Foldable Arguments

deriving instance Foldable RunArgs

deriving instance Foldable CheckArgs

deriving instance Foldable Check

deriving instance Foldable Instruction

deriving instance Foldable InstructionPos

-- Applies the same right fold to three nested foldables
foldr3 :: (Foldable t1, Foldable t2, Foldable t3) => (a -> b -> b) -> b -> t1 (t2 (t3 a)) -> b
foldr3 = foldr . flip . foldr . flip . foldr

unnestToken :: Token -> InnerToken Token
unnestToken (OuterToken _ t) = t

-- Custom fold method for tokens so the inner tokens nested in tokens are also folded.
foldTokens :: (InnerToken Token -> a -> a) -> a -> Token -> a
foldTokens f acc t = (foldr . flip . foldTokens) f (f (unnestToken t) acc) (unnestToken t)

concatTokens :: Token -> [InnerToken Token]
concatTokens = foldTokens (:) []

anyToken :: (InnerToken Token -> Bool) -> Token -> Bool
anyToken f = any f . concatTokens

anyTokenOfInstruction :: (InnerToken Token -> Bool) -> InstructionPos (Maybe Token) -> Bool
anyTokenOfInstruction = any . any . anyToken

anyInstruction :: (Instruction a -> Bool) -> InstructionPos a -> Bool
anyInstruction f InstructionPos {instruction} = f instruction

fmapToken :: (InnerToken Token -> InnerToken Token) -> Token -> Token
fmapToken f (OuterToken id innerToken) = OuterToken id $ f $ fmap (fmapToken f) innerToken

-- Fold tokens until the predicate is fulfilled. Throw away everything stating from the first token that fulfills the predicate.
foldTokensUntil :: (InnerToken Token -> Bool) -> (InnerToken Token -> a -> a) -> a -> Token -> a
foldTokensUntil predicate _ acc t | predicate (unnestToken t) = acc
foldTokensUntil predicate f acc t = (foldr . flip . foldTokensUntil predicate) f (f (unnestToken t) acc) (unnestToken t)

-- Concat all tokens until the predicate is fulfilled. The token fulfilling the predicate will not be included.
concatTokensUntil :: (InnerToken Token -> Bool) -> Token -> [InnerToken Token]
concatTokensUntil predicate = foldTokensUntil predicate (:) []

fmapTokensUntil :: (InnerToken Token -> Bool) -> (InnerToken Token -> InnerToken Token) -> Token -> Token
fmapTokensUntil predicate f (OuterToken id innerToken) | predicate innerToken = OuterToken id innerToken
fmapTokensUntil predicate f (OuterToken id innerToken) = OuterToken id $ f $ fmap (fmapTokensUntil predicate f) innerToken

mapTokensUntil :: (InnerToken Token -> Bool) -> (InnerToken Token -> b) -> Token -> [b]
mapTokensUntil predicate f = map f . concatTokensUntil predicate

fmapDockerfileInstructions :: (Instruction a -> Instruction b) -> [InstructionPos a] -> [InstructionPos b]
fmapDockerfileInstructions = map . fmapInstruction

fmapInstruction :: (Instruction a -> Instruction b) -> InstructionPos a -> InstructionPos b
fmapInstruction f x@InstructionPos {instruction = instruction} = x {instruction = f instruction}

fmapCommands :: Command a => (a -> a) -> Instruction (Maybe Token) -> Instruction (Maybe Token)
fmapCommands = fmap . fmap . fmapToken . fmapCommand

-- convert an Instruction to an InstructionPos with arbitrary line number and source file
instructionToInstructionPos :: Instruction a -> InstructionPos a
instructionToInstructionPos instruction = InstructionPos {instruction = instruction, sourcename = "", lineNumber = 0}

-- Create a simple RUN instruction that includes an `a`.
toRunInstruction :: a -> Instruction a
toRunInstruction x = Run (RunArgs (ArgumentsText x) (RunFlags empty Nothing Nothing))

-- Concatinate multiple bash pipelines tokens into a single bash script token.
toBashScript :: [Token] -> Token
toBashScript = OuterToken (Id 0) . Inner_T_Annotation [] . OuterToken (Id 1) . Inner_T_Script (OuterToken (Id 2) (Inner_T_Literal ""))

-- Convert a command to a bash pipeline
toPipeline :: Command a => a -> Token
toPipeline command = OuterToken (Id 0) $ Inner_T_Pipeline [] [OuterToken (Id 1) $ Inner_T_Redirecting [] $ OuterToken (Id 2) (toAST command)]

-- Convert a command to a InstructionPos that is a RUN instruction executing the command.
toInstruction :: Command a => a -> InstructionPos (Maybe Token)
toInstruction command = InstructionPos {instruction = toRunInstruction $ Just $ toBashScript [toPipeline command], sourcename = "", lineNumber = 0}

mapInstructionsToMany :: (InstructionPos a -> [InstructionPos a]) -> [InstructionPos a] -> [InstructionPos a]
mapInstructionsToMany f = foldr (\instruction acc -> f instruction ++ acc) []

mapToManyAfter :: (a -> Bool) -> (a -> [a]) -> [a] -> [a]
mapToManyAfter predicate f = snd . foldr helper (False, [])
  where
    helper x (False, xs) | predicate x = (True, x : xs)
    helper x (False, xs) = (False, f x ++ xs)
    helper x (True, xs) = (True, x : xs)

word8sToWord32 :: [Word8] -> Word32
word8sToWord32 = foldl' accum 0
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o

hashToInt :: Crypto.Hash.Digest MD5 -> Int
hashToInt a = fromIntegral $ word8sToWord32 $ BA.unpack a

hashDockerfile :: Text -> Crypto.Hash.Digest MD5
hashDockerfile = Crypto.Hash.hash . encodeUtf16LE

seedFromDockerfileText :: Text -> Int
seedFromDockerfileText = hashToInt . hashDockerfile
