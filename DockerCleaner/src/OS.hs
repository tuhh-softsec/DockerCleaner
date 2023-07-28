{-# LANGUAGE NamedFieldPuns #-}

module OS where

import Dockerfile (Dockerfile)
import Language.Docker (InstructionPos (InstructionPos), instruction, Instruction (From), BaseImage (BaseImage), image, Image (Image), imageName)
import qualified Data.Text as T
import Data.List (isSubsequenceOf)
import Control.Applicative ((<|>))

data OS = Linux | Windows

guessOS :: Dockerfile -> Maybe OS
guessOS (x : xs) =  helper x <|> guessOS xs where
  helper :: InstructionPos a -> Maybe OS
  helper InstructionPos {instruction = From BaseImage {image = Image {imageName}}} = guessOsFromImageName $ T.unpack imageName
  helper _ = Nothing
guessOS [] = Nothing

guessOsFromImageName :: String -> Maybe OS
guessOsFromImageName name | "microsoft" `isSubsequenceOf` name || "windows" `isSubsequenceOf` name = Just Windows
guessOsFromImageName _ = Just Linux
