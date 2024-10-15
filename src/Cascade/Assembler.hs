module Cascade.Assembler
(
  OpCode (..),
  assembleByteCode
) where

{- Imports -}
import qualified Data.Map as Map (Map, fromList, lookup)
import qualified Data.Either as Eith (lefts, rights)
import Data.Foldable (Foldable(foldl'))

data OpCode = NOP   -- Do nothing
            | HALT  -- Stop the CPU
            | PUSH  -- Push a value onto the stack
            | POP   -- Pop the most recent value off the stack
            | ADD   -- Pop two values a and b and push b + a
            | SUB   -- Pop two values a and b and push b - a
            | ILL   -- Intentionally illegal opcode
            deriving (Show, Eq, Ord, Enum, Bounded)

type OpAndParam = (OpCode, [Int])

data Instruction = Instr { opcode    :: OpCode
                         , pop_cnt   :: Int
                         , param_cnt :: Int
                         }
                   deriving (Show, Eq)

instructions :: [Instruction]
instructions = [ Instr { opcode = NOP, pop_cnt = 0, param_cnt = 0}
               , Instr { opcode = HALT, pop_cnt = 0, param_cnt = 0}
               , Instr { opcode = PUSH, pop_cnt = 0, param_cnt = 1}
               , Instr { opcode = POP, pop_cnt = 1, param_cnt = 0}
               , Instr { opcode = ADD, pop_cnt = 2, param_cnt = 0}
               , Instr { opcode = SUB, pop_cnt = 2, param_cnt = 0}
               ]

instrByOp :: Map.Map OpCode Instruction
instrByOp = Map.fromList $ map (\i -> (opcode i, i)) instructions

{- Assembler details -}
data AssemblerError = IllegalOpCodeError Integer OpAndParam
                    | ParamCountError Integer OpAndParam
                    deriving (Show, Eq)

type AsmErrorOrByteCode = [Either AssemblerError [Int]]
assemble :: AsmErrorOrByteCode -> OpAndParam -> AsmErrorOrByteCode
assemble result instr@(op, params) = 
  result ++ case Map.lookup op instrByOp of
    Nothing -> 
      [Left $ IllegalOpCodeError location instr]
    Just i ->
      if param_cnt i == length params
      then [Right $ fromEnum (opcode i) : params]
      else [Left $ ParamCountError location instr]
  where location = toInteger $ length result + 1

assembleByteCode :: [OpAndParam] -> Either [AssemblerError] [Int]
assembleByteCode code = 
  let result = foldl' assemble [] code in
  case Eith.lefts result of
    [] -> Right $ concat $ Eith.rights result
    errors -> Left errors

