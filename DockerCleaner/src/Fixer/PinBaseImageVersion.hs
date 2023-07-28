{-# LANGUAGE NamedFieldPuns #-}

module Fixer.PinBaseImageVersion where

import Helper
import Language.Docker (BaseImage (digest, tag, BaseImage, image), Instruction (From), InstructionPos (InstructionPos), Tag (Tag), Image (Image), image, imageName, instruction)
import System.Random (RandomGen)
import qualified Data.Text as T
import Data.Map (findWithDefault, Map)
import PackageVersions (PackageVersions)
import Data.Maybe (mapMaybe)
import Data.List (find)

newtype BaseImageVersions = BaseImageVersions PackageVersions

getVersion :: BaseImageVersions -> Image -> Maybe Tag
getVersion (BaseImageVersions versions) Image {imageName} = fmap (Tag . T.pack) $ find (\x -> x == (T.unpack imageName)) versions 

getBaseImages :: [InstructionPos a] -> [String]
getBaseImages = mapMaybe helper where
  helper InstructionPos {instruction=(From x@BaseImage {image = Image {imageName}})} = Just $ T.unpack imageName
  helper _ = Nothing

fix :: (Image -> Maybe Tag) -> [InstructionPos a] -> [InstructionPos a]
fix getVersion = fmapDockerfileInstructions helper
  where
    helper (From x@BaseImage {image, tag = Nothing}) = From x {tag = getVersion image}
    helper x = x
