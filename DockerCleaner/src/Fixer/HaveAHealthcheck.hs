{-# LANGUAGE OverloadedStrings #-}

module Fixer.HaveAHealthcheck where

import Helper
import Language.Docker (Instruction (Comment, Healthcheck), InstructionPos (instruction), parseText)
import Language.Docker.Syntax hiding (Dockerfile)
import ShellCheck.AST (Token)
import Dockerfile (Dockerfile)
import PrintShellAST (parseShell, printShellText, printToken)
import Fixer.PinBaseImageVersion (getBaseImages)

isHealthCheck :: Instruction a -> Bool
isHealthCheck Healthcheck {} = True
isHealthCheck _ = False

includesHealthCheck :: [InstructionPos a] -> Bool
includesHealthCheck = any $ anyInstruction isHealthCheck

createHealthCheckForCommand :: String -> Instruction (Maybe Token)
createHealthCheckForCommand command = Healthcheck (Check (CheckArgs {checkCommand = ArgumentsText (parseShell command), interval = Nothing, timeout = Nothing, startPeriod = Nothing, retries = Nothing}))

healthCheckReminder :: Dockerfile -> [Instruction (Maybe Token)]
healthCheckReminder dockerfile = do
    let image = head $ getBaseImages dockerfile
    case image of
        "tomcat" -> [createHealthCheckForCommand "curl --fail http://127.0.0.1:8080 || exit 1"]
        "php" -> [createHealthCheckForCommand "curl --fail http://127.0.0.1:80 || exit 1"]
        "apache" -> [createHealthCheckForCommand "curl --fail http://127.0.0.1:80 || exit 1"]
        "nginx" -> [createHealthCheckForCommand "curl --fail http://127.0.0.1:80 || exit 1"]
        "node" -> [createHealthCheckForCommand "curl --fail http://127.0.0.1:3000 || exit 1"]
        -- should check also for the EXPOSE instruction
        _ -> [Comment "Please add your HEALTHCHECK here!!!"]

fix :: Dockerfile -> Dockerfile
fix instructions | includesHealthCheck instructions = instructions
fix instructions = instructions ++ map instructionToInstructionPos (healthCheckReminder (instructions))
