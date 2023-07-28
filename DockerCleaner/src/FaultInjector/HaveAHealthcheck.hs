module FaultInjector.HaveAHealthcheck where

import Language.Docker (Instruction (Healthcheck), InstructionPos (InstructionPos), instruction)
import qualified Language.Docker as FaultInjector
import System.Random (RandomGen)
import Helper

injectFault :: RandomGen g => g -> [InstructionPos a] -> [InstructionPos a]
injectFault _ = mapInstructionsToMany helper
  where
    helper InstructionPos {instruction = (Healthcheck _)} = []
    helper x = [x]
