{-# LANGUAGE OverloadedStrings #-}

module FaultInjector.HaveAUser where

import Language.Docker (Instruction (User), InstructionPos (InstructionPos, instruction), instruction)
import qualified Language.Docker as FaultInjector
import System.Random (RandomGen)
import Helper
import qualified Data.Text as T
import Data.Text (Text)
import RandomHelper (vectorOfRange, (|++|), (++|), (|++), fromGen)
import Test.QuickCheck (suchThat, Arbitrary (arbitrary), vectorOf, oneof, elements)
import Data.Char (isLower, isNumber, isAscii)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

hasGroup :: Text -> Bool
hasGroup = T.any (== ':')

removeGroup :: Text -> Text
removeGroup = T.takeWhile (/= ':')

isRoot :: T.Text -> Bool
isRoot "root" = True
isRoot "0" = True
isRoot name | hasGroup name = isRoot $ removeGroup name
isRoot _ = False

isGroupNameChar a = isAscii a && (isLower a || isNumber a || a == '_' || a == '-')
isFirstGroupNameChar a = isAscii a && (isLower a || a == '_')

--arbitraryGroupName = vectorOf 1 (suchThat arbitrary isFirstGroupNameChar) |++| vectorOfRange (0, 15) (suchThat arbitrary isGroupNameChar)
arbitraryRootUserName = elements ["0", "root"]

arbitraryRootUser = arbitraryRootUserName

arbitraryRootUserInstruction = instructionToInstructionPos . User . T.pack <$> arbitraryRootUser

isUserInstruction :: InstructionPos a -> Bool
isUserInstruction InstructionPos {instruction = (User _)} = True
isUserInstruction _ = False

isRootUserInstruction :: InstructionPos a -> Bool
isRootUserInstruction InstructionPos {instruction = (User user)} = isRoot user
isRootUserInstruction _ = False

-- Is the last user instruction a root user instruction? When no user instructions exist this returns Nothing.
lastUserInstructionIsRootUserInstruction :: [InstructionPos a] -> Maybe Bool
lastUserInstructionIsRootUserInstruction (x : xs) | isRootUserInstruction x = lastUserInstructionIsRootUserInstruction xs <|> Just True
lastUserInstructionIsRootUserInstruction (x : xs) | isUserInstruction x = lastUserInstructionIsRootUserInstruction xs <|> Just False
lastUserInstructionIsRootUserInstruction (_ : xs) = lastUserInstructionIsRootUserInstruction xs
lastUserInstructionIsRootUserInstruction [] = Nothing

injectFault :: RandomGen g => g -> [InstructionPos a] -> [InstructionPos a]
-- The last USER instructions defines a root user -> Do nothing
injectFault _ instructions | fromMaybe False $ lastUserInstructionIsRootUserInstruction instructions = instructions
-- The last USER instruction defines a non root user -> Change to a root user at the end
injectFault gen instructions | not $ fromMaybe True $ lastUserInstructionIsRootUserInstruction instructions = instructions ++ [fst $ fromGen arbitraryRootUserInstruction gen]
-- No USER instructions exists -> Add a root user at the end or do nothing
injectFault gen instructions = instructions ++ fst (fromGen (vectorOfRange (0, 1) arbitraryRootUserInstruction) gen)
