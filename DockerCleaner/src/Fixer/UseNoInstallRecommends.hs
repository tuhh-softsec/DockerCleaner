module Fixer.UseNoInstallRecommends where

import Command.AptGet (AptGet (AptGetInstall, noInstallRecommends))
import Helper
import Language.Docker (InstructionPos)
import ShellCheck.AST (Token)
import Dockerfile (Dockerfile)

fix :: Dockerfile -> Dockerfile
fix = fmapDockerfileInstructions $ fmapCommands helper
  where
    helper :: AptGet -> AptGet
    helper x@AptGetInstall {noInstallRecommends = False} = x {noInstallRecommends = True}
    helper x = x
