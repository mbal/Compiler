{-# OPTIONS_GHC -Wall #-}

-- This module contains the code to:
-- 1. fix the jump targets in the assembly.
--
-- To fix the targets of the jumps we have to:
-- a. walk over the code; JUMP, during this phase, have an increasing
--    index
-- b. if the current instruction is a jump, look up the target in the
--    label maps (maps between label and instruction position in the
--    bytecode).
-- c. if the jump is absolute, leave it.
-- d. if the jump is relative, compute the offset as
--    (target - currentPosition - currentInstructionSize), and modify
--    the instruction.

module Assemble where
import Data.Word (Word16)
import qualified Data.Map as Map

import Bytecode
import Emit
import Types (Instruction(..), AugInstruction(..), CompilerState(..), CodeBlock(..), LabelID, getBlockState, modifyBlockState)

-- main purpose of this function is to fix jump targets
assemble :: CompilerState ()
assemble = do
  lblMap <- getBlockState block_labelMap
  code <- fmap reverse (getBlockState block_instructions)
  let annotatedCode = fixJumpTarget lblMap code
  modifyBlockState $ \s -> s { block_instructions = annotatedCode }

fixJumpTarget :: Map.Map LabelID Word16 -> [AugInstruction] -> [AugInstruction]
fixJumpTarget labelMap code =
  -- walk over the code, if an instruction is a jump, look it up in the
  -- label map, compute the offset (if relative) and fix the target
  map (\x -> fixJump labelMap x) code

fixJump :: Map.Map LabelID Word16 -> AugInstruction -> AugInstruction
fixJump mp augInstr@(AugInstruction (Instruction opcode arg) idx) =
  if isJump opcode then
    case arg of
      Just x ->
        let jmpTarget = Map.lookup x mp
            newArg = case jmpTarget of
              Just k -> computeTarget k idx (instr augInstr)
              Nothing -> error "shouldn't happen"
            in augInstr { instr = (Instruction opcode (Just newArg)) }
      Nothing -> error $ "shouldn't happen"
  else augInstr

computeTarget :: Word16 -> Word16 -> Instruction -> Word16
computeTarget target instrIndex inst@(Instruction opcode _) =
  if isRelativeJump opcode then
    (target - (instrIndex + (fromIntegral $ instructionSize inst)))
  else
    target
