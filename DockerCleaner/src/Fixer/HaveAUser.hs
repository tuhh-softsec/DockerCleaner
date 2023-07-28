{-# LANGUAGE OverloadedStrings #-}

module Fixer.HaveAUser where

import Dockerfile (Dockerfile)
import Helper
import Language.Docker (Instruction (Comment, Healthcheck, User, From), InstructionPos (instruction, InstructionPos), BaseImage (BaseImage), image, Image (Image), imageName)
import ShellCheck.AST (Token)
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Command (Command(parse))
import qualified Command.Groupadd as G
import qualified Command.Addgroup as G2
import qualified Command.Useradd as U
import qualified Command.Adduser as U2
import Data.List (isSubsequenceOf)

hasGroup :: T.Text -> Bool
hasGroup = T.any (== ':')

removeGroup :: T.Text -> T.Text
removeGroup = T.takeWhile (/= ':')

isRoot :: T.Text -> Bool
isRoot "root" = True
isRoot "0" = True
isRoot name | hasGroup name = isRoot $ removeGroup name
isRoot _ = False

isUserInstruction :: InstructionPos a -> Bool
isUserInstruction InstructionPos {instruction = (User _)} = True
isUserInstruction _ = False

isRootUserInstruction :: InstructionPos a -> Bool
isRootUserInstruction InstructionPos {instruction = (User user)} = isRoot user
isRootUserInstruction _ = False

-- Is the last user instruction using a root user? When no user instructions exist this returns Nothing.
lastUserIsRootUser :: Dockerfile -> Maybe Bool
lastUserIsRootUser (x : xs) | isRootUserInstruction x = lastUserIsRootUser xs <|> Just True
lastUserIsRootUser (x : xs) | isUserInstruction x = lastUserIsRootUser xs <|> Just False
lastUserIsRootUser (_ : xs) = lastUserIsRootUser xs
lastUserIsRootUser [] = Nothing

lastUserIsRootUserOrNotDefined :: Dockerfile -> Bool
lastUserIsRootUserOrNotDefined = fromMaybe True . lastUserIsRootUser

-- RUN groupadd -r docker-user && useradd --no-log-init -r -g docker-user docker-user
createLinuxUser :: Instruction (Maybe Token)
createLinuxUser = toRunInstruction $ Just $ toBashScript [toPipeline $ G.defaultGroupadd {G.group = "docker-user", G.system = True}, toPipeline $ U.defaultUseradd {U.name = "docker-user", U.gid = Just "docker-user", U.system = True}]

createAlpineLinuxUser :: Instruction (Maybe Token)
createAlpineLinuxUser = toRunInstruction $ Just $ toBashScript [toPipeline $ G2.defaultAddgroup {G2.group = "docker-user", G2.system = True}, toPipeline $ U2.defaultAdduser {U2.name = "docker-user", U2.gid = Just "docker-user", U2.system = True}]

newUserInstruction :: Instruction a
newUserInstruction = User "docker-user"

fix :: (String, String) -> Dockerfile -> Dockerfile
fix osImage instructions | lastUserIsRootUserOrNotDefined instructions = do
    let imgName = fst osImage
    case imgName of
        "scratch" -> instructions
        "alpine" -> instructions ++ [instructionToInstructionPos createAlpineLinuxUser, instructionToInstructionPos newUserInstruction]
        _ -> instructions ++ [instructionToInstructionPos createLinuxUser, instructionToInstructionPos newUserInstruction]

fix _ instructions = instructions
