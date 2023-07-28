module FaultInjector.UseNoInstallRecommends where

import Command.AptGet (AptGet (AptGetInstall, noInstallRecommends))
import Helper
import Language.Docker (InstructionPos)
import ShellCheck.AST (Token)
import System.Random (RandomGen)
import Dockerfile (Dockerfile)

injectFault :: RandomGen g => g -> Dockerfile -> Dockerfile
injectFault _ = fmapDockerfileInstructions $ fmapCommands helper
  where
    helper x@AptGetInstall {noInstallRecommends = True} = x {noInstallRecommends = False}
    helper x = x
