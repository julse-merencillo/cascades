{-# LANGUAGE PatternSynonyms #-}

-- | This module contains Cascade's runtime.
module Cascade.Runtime
(
    OpCode(..),
    Program,
    runProgram,
) where

import qualified Data.Bits     as B
import qualified Data.Sequence as S

import Data.Word
import System.Exit
import System.IO

-- ( Runtime Options ) --------------------------------------------------------

-- | The size of each memory cell, as well as the addressable memory space.
type Cell = Word16

-- | The addressable instruction space.
type Addr = Int

-- | Default program memory capacity.
memCapacity :: Word16
memCapacity = maxBound

-- ( Instruction Set ) --------------------------------------------------------

-- | Cascade's instruction set.
data OpCode = NOP       -- ^ Do nothing
            | END       -- ^ Terminate the program
            | ILL       -- ^ Intentionally illegal opcode for testing only

            | PSH Cell  -- ^ Store a value onto the stack
            | POP       -- ^ Remove the top value from the stack
            | DUP       -- ^ Duplicate the top value on the stack

            | LVR Cell  -- ^ Load a value onto the register
            | LDR       -- ^ Load a value from memory to the register
            | STR       -- ^ Store the register to memory

            | PNT Cell  -- ^ Point to a memory address
            | INC       -- ^ Increment the memory pointer
            | DEC       -- ^ Decrement the memory pointer

            | SWP       -- ^ Swap the top value and register
            | RGS       -- ^ Move the value from the register to the stack
            | SRG       -- ^ Move the top value from the stack to the register

            | ADD       -- ^ Compute the sum of the top two values
            | SUB       -- ^ Compute the difference of the top two values
            | MUL       -- ^ Compute the product of the top two values
            | DIV       -- ^ Compute the quotient of the top two values
            | MOD       -- ^ Compute the modulo of the top two values

            | AND       -- ^ Compute the logical AND of the top two values
            | IOR       -- ^ Compute the logical OR of the top two values
            | XOR       -- ^ Compute the logical XOR of the top two values
            | NOT       -- ^ Compute the logical NOT of the top value
            | SHL Cell  -- ^ Shift the bits of the top value to the left
            | SHR Cell  -- ^ Shift the bits of the top value to the right
            | CNT       -- ^ Push the number of 1 bits in the top value

            | JMP Addr  -- ^ Jump to the specified program address
            | JZE Addr  -- ^ Jump if the top value is 0
            | JNZ Addr  -- ^ Jump if the top value is not 0
            | JEQ Addr  -- ^ Jump if the top value and register are equal
            | JNQ Addr  -- ^ Jump if the top value and register are not equal
            | JGT Addr  -- ^ Jump if the top value is greater than the register
            | JLT Addr  -- ^ Jump if the top value is lesser than the register
            deriving (Show)

-- | A list of instructions to be executed by the machine.
type Program = S.Seq OpCode

-- ( Memory and Program State ) -----------------------------------------------

-- | Represents program memory.
type Memory = S.Seq Cell

-- | Contains program state during execution.
data ExecUnit = Unit { ip :: Addr       -- ^ Instruction pointer
                     , mem_p :: Cell    -- ^ Memory pointer
                     , reg   :: Cell    -- ^ Program register
                     , stack :: [Cell]  -- ^ Program stack
                     , mem_u :: Memory  -- ^ Program memory
                     }
                deriving (Show)

-- | Initally empty memory, initialized with the max capacity.
emptyMem :: Memory
emptyMem = S.replicate (fromIntegral memCapacity) 0

-- | Initial program state before execution.
initState :: ExecUnit
initState = Unit { ip = 0
                 , mem_p = 0
                 , reg   = 0
                 , stack = []
                 , mem_u = emptyMem }

-- | Helper function to grab the first item in the stack
first :: ExecUnit -> Cell
first = (head . stack)

-- | Helper function to grab the rest of the stack
rest :: ExecUnit -> [Cell]
rest = (tail . stack)

-- ( Runtime Status and Error Codes ) -----------------------------------------

-- | Errors that could occur during runtime, along with the error code
data RuntimeError = EmptyProgram     -- ^ (166) Runtime given an empty program
                  | InstOutOfBounds  -- ^ (167) `ip` is out of bounds
                  | DivByZero        -- ^ (168) Division by zero
                  | NotEnoughItems   -- ^ (169) Not enough items in stack
                  | IntentionalEnd   -- ^ (170) Attempting to run ILL
                  | MemOutOfBounds   -- ^ (171) `mem_p` is out of bounds
                  | UnknownError     -- ^ (199) Error of unknown nature
                  
-- | Defines a relationship between runtime errors and error codes.
instance Enum RuntimeError where
    fromEnum x = case x of
        EmptyProgram    -> 166
        InstOutOfBounds -> 167
        DivByZero       -> 168
        NotEnoughItems  -> 169
        IntentionalEnd  -> 170
        MemOutOfBounds  -> 171
        UnknownError    -> 199
    toEnum x = case x of
        166 -> EmptyProgram
        167 -> InstOutOfBounds
        168 -> DivByZero
        169 -> NotEnoughItems
        170 -> IntentionalEnd
        171 -> MemOutOfBounds
        _   -> UnknownError

-- | Error messages for each possible runtime error.
instance Show RuntimeError where
    show EmptyProgram    = "The runtime was given an empty program."
    show InstOutOfBounds = "The instruction pointer is out of bounds."
    show DivByZero       = "The runtime tried dividing by 0."
    show NotEnoughItems  = "There are not enough values in the stack."
    show IntentionalEnd  = "This error is intentionally triggered."
    show MemOutOfBounds  = "The memory pointer is out of bounds."
    show UnknownError    = "The cause of this error is unknown."

-- | Outputs an error message, and exits with the corresponding error code.
reportError :: RuntimeError -> IO ()
reportError err = do
    hPutStrLn stderr $ "Runtime Error | " ++ show err
    exitWith $ ExitFailure (fromEnum err)

-- | Indicates the status of the current program.
data Status = OK | ERR RuntimeError | EXIT

-- ( Program Execution ) ------------------------------------------------------

-- | Runs the program in a fetch-decode-execute cycle.
runProgram :: Program -> IO () 
runProgram S.Empty = reportError EmptyProgram
runProgram prog    = fetch prog initState

-- | Fetches the instruction pointed at by the instruction pointer. If none
--   exists, an error is thrown.
fetch :: Program -> ExecUnit -> IO ()
fetch prog state = case S.lookup (ip state) prog of
    Nothing -> reportError InstOutOfBounds
    Just op -> decode prog op state

-- | Prepares for execution by checking if the program state won't lead to any
--   errors. Otherwise, throw the relevant errors.
decode :: Program -> OpCode -> ExecUnit -> IO ()
decode prog op state = case check op state of
    OK      -> execute prog op state
    ERR err -> reportError err
    EXIT    -> exitWith ExitSuccess

-- | Executes the instruction and fetches the next instruction.
execute :: Program -> OpCode -> ExecUnit -> IO ()
execute prog op state = do
    newState <- runOp op state
    print newState
    fetch prog newState

-- ( Error Checking ) ---------------------------------------------------------

-- | Pattern that checks if 0 is on top of the stack
pattern ZeroOnTop :: ExecUnit
pattern ZeroOnTop <- Unit { stack = (0:_) }

-- | Pattern that checks if the stack is empty
pattern EmptyStack :: ExecUnit
pattern EmptyStack <- Unit { stack = [] }

-- | Pattern that checks if only one item is in the stack
pattern OneInStack :: ExecUnit
pattern OneInStack <- Unit { stack = (_:[]) }

-- | Checks any possible error conditions for each opcode. Throws the relevant
--   error if it occurs.
check :: OpCode -> ExecUnit -> Status

-- The stack is empty, but the operation needs an item in the stack
check POP     EmptyStack = ERR NotEnoughItems
check DUP     EmptyStack = ERR NotEnoughItems
check SWP     EmptyStack = ERR NotEnoughItems
check SRG     EmptyStack = ERR NotEnoughItems
check ADD     EmptyStack = ERR NotEnoughItems
check SUB     EmptyStack = ERR NotEnoughItems
check MUL     EmptyStack = ERR NotEnoughItems
check DIV     EmptyStack = ERR NotEnoughItems
check MOD     EmptyStack = ERR NotEnoughItems
check AND     EmptyStack = ERR NotEnoughItems
check IOR     EmptyStack = ERR NotEnoughItems
check XOR     EmptyStack = ERR NotEnoughItems
check NOT     EmptyStack = ERR NotEnoughItems
check (SHL _) EmptyStack = ERR NotEnoughItems
check (SHR _) EmptyStack = ERR NotEnoughItems
check CNT     EmptyStack = ERR NotEnoughItems
check (JZE _) EmptyStack = ERR NotEnoughItems
check (JNZ _) EmptyStack = ERR NotEnoughItems
check (JEQ _) EmptyStack = ERR NotEnoughItems
check (JNQ _) EmptyStack = ERR NotEnoughItems
check (JGT _) EmptyStack = ERR NotEnoughItems
check (JLT _) EmptyStack = ERR NotEnoughItems

-- The stack only contains one item, but the operation needs two items
check ADD OneInStack = ERR NotEnoughItems
check SUB OneInStack = ERR NotEnoughItems
check MUL OneInStack = ERR NotEnoughItems
check DIV OneInStack = ERR NotEnoughItems
check MOD OneInStack = ERR NotEnoughItems
check AND OneInStack = ERR NotEnoughItems
check IOR OneInStack = ERR NotEnoughItems
check XOR OneInStack = ERR NotEnoughItems

-- Memory pointer is pointing to something out of bounds
check LDR st | mem_p st >= mem_len = ERR MemOutOfBounds
             | otherwise           = OK
             where mem_len = fromIntegral $ S.length (mem_u st)
check STR st | mem_p st >= mem_len = ERR MemOutOfBounds
             | otherwise           = OK
             where mem_len = fromIntegral $ S.length (mem_u st)

-- Division by zero
check DIV ZeroOnTop = ERR DivByZero
check MOD ZeroOnTop = ERR DivByZero

-- Program termination
check END _ = EXIT
check ILL _ = ERR IntentionalEnd

-- Only run this after checking every other opcode that can fail.
check _ _ = OK

-- ( Instruction Execution ) --------------------------------------------------

-- | Performs the actual computation and returns the state after execution.
runOp :: OpCode -> ExecUnit -> IO ExecUnit

-- Stack instructions
runOp (PSH x) st = push st x
runOp POP     st = return $ st { stack = rest st, ip = ip st + 1 }
runOp DUP     st = push st $ first st

-- Register-memory manipulation
runOp (LVR x) st = return $ st { reg = x, ip = ip st + 1 }
runOp LDR     st = case S.lookup (fromIntegral (mem_p st)) (mem_u st) of
    Nothing -> return $ st { reg = 0, ip = ip st + 1 }
    Just x  -> return $ st { reg = x, ip = ip st + 1 }
runOp STR     st = return $ st { mem_u = new_mem, ip = ip st + 1 }
    where new_mem = S.update (fromIntegral $ mem_p st) (reg st) (mem_u st)

-- Memory pointer manipulation
runOp (PNT x) st = return $ st { mem_p = x, ip = ip st + 1 }
runOp INC     st = return $ st { mem_p = mem_p st + 1, ip = ip st + 1 }
runOp DEC     st = return $ st { mem_p = mem_p st - 1, ip = ip st + 1 }

-- Register-stack manipulation
runOp SWP st = return $ st { reg = first st, stack = newStack, ip = ip st + 1 }
    where newStack = reg st : rest st
runOp RGS st = return $ st { reg = 0, stack = newStack, ip = ip st + 1 }
    where newStack = reg st : rest st
runOp SRG st = return $ st { reg = first st, stack = rest st, ip = ip st + 1 }

-- Arithmetic operations
runOp ADD st = calc st (+)
runOp SUB st = calc st (-)
runOp MUL st = calc st (*)
runOp DIV st = calc st div
runOp MOD st = calc st mod

-- Bitwise operations
runOp AND st = calc st (B..&.)
runOp IOR st = calc st (B..|.)
runOp XOR st = calc st B.xor
runOp NOT st = return $ st { stack = (B.complement . first) st : rest st
                           , ip    = ip st + 1 }
runOp CNT st = push st $ (fromIntegral count :: Cell)
    where count = B.popCount $ ((fromIntegral . first) st :: Int)

-- Shift instructions
runOp (SHL x) st = return $ st { stack = (B.shift res xn) : rest st
                               , ip    = ip st + 1 }
    where res    = first st
          xn     = fromIntegral x
runOp (SHR x) st = return $ st { stack = (B.shift res (-xn)) : rest st
                               , ip    = ip st + 1 }
    where res    = first st
          xn     = fromIntegral x

-- Branching instructions
runOp (JMP x) st = jump st x $ True
runOp (JZE x) st = jump st x $ (first st) == 0
runOp (JNZ x) st = jump st x $ (first st) /= 0
runOp (JEQ x) st = jump st x $ (first st) == (reg st)
runOp (JNQ x) st = jump st x $ (first st) /= (reg st)
runOp (JGT x) st = jump st x $ (first st) > (reg st)
runOp (JLT x) st = jump st x $ (first st) < (reg st)

-- Should an opcode not have a previous definition, it is treated as a NOP
runOp _ st = return $ st { ip = ip st + 1 }

-- | Pushes a number to the stack.
push :: ExecUnit -> Cell -> IO ExecUnit
push st x = return $ st { stack = x : stack st, ip = ip st + 1 }

-- | Performs an arithmetic or bitwise operation.
calc :: ExecUnit -> (Cell -> Cell -> Cell) -> IO ExecUnit
calc st f = let a = first st
                b = rest st
            in return $ st { stack = (head b `f` a) : tail b, ip = ip st + 1 }

-- | Performs a branch instruction.
jump :: ExecUnit -> Addr -> Bool -> IO ExecUnit
jump st x True  = return $ st { ip = x }
jump st _ False = return $ st { ip = ip st + 1 }
