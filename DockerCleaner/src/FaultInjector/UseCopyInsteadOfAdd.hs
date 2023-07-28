module FaultInjector.UseCopyInsteadOfAdd where

import Helper
import Language.Docker (AddArgs (AddArgs), CopyArgs (CopyArgs, sourceFlag), CopySource (NoSource), Instruction (Add, Copy), InstructionPos)
import System.Random (RandomGen)

copyArgsToAddArgs :: CopyArgs -> AddArgs
copyArgsToAddArgs (CopyArgs sourcePaths targetPath chownFlag chmodFlag _) = AddArgs sourcePaths targetPath chownFlag chmodFlag

injectFault :: RandomGen g => g -> [InstructionPos a] -> [InstructionPos a]
injectFault _ = fmapDockerfileInstructions helper
  where
    helper (Copy args@CopyArgs {sourceFlag = NoSource}) = Add (copyArgsToAddArgs args)
    helper x = x
