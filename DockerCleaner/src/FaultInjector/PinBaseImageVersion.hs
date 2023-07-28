module FaultInjector.PinBaseImageVersion where

import Helper
import Language.Docker (BaseImage (digest, tag), Instruction (From), InstructionPos)
import System.Random (RandomGen)

injectFault :: RandomGen g => g -> [InstructionPos a] -> [InstructionPos a]
injectFault _ = fmapDockerfileInstructions helper
  where
    helper (From x) = From x {tag = Nothing, digest = Nothing}
    helper x = x
