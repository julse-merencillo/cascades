{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, PatternSynonyms #-}

-- | This module contains Cascade's runtime; opcodes, instructions, and the
--   interpreter.
module Cascade.Runtime
(
  OpCode (..),
  Addr,
  InstructionList,
  CPU,
  execute
) where

import qualified Data.Sequence as S
import qualified Data.Text     as T
import qualified Data.Bits     as B
default (Int, T.Text)

-- | Currently available assembly instructions.
data OpCode = NOP          -- ^ Do nothing
            | END          -- ^ Terminate the program
            | ILL          -- ^ Intentionally illegal opcode for testing

            -- Stack manipulation
            | PSH Int      -- ^ Push a number onto the stack
            | POP          -- ^ Remove the top number from the stack
            | DUP          -- ^ Duplicate the top number on the stack

            -- Numerical operations
            | ADD          -- ^ Pop two numbers @a@ and @b@, then push @b + a@
            | SUB          -- ^ Pop two numbers @a@ and @b@, then push @b - a@
            | MUL          -- ^ Pop two numbers @a@ and @b@, then push @b * a@
            | DIV          -- ^ Pop two numbers @a@ and @b@, then push @b / a@
            | MOD          -- ^ Pop two numbers @a@ and @b@, then push @b mod a@

            -- Bitwise operations
            | AND          -- ^ Pop two numbers @a@ and @b@, then push @b AND a@
            | IOR          -- ^ Pop two numbers @a@ and @b@, then push @b OR a@
            | XOR          -- ^ Pop two numbers @a@ and @b@, then push @b XOR a@
            | NOT          -- ^ Pop the top number and push its complement
            | SHL Int      -- ^ Pop the top number and push it back, with its
                           --   bits shifted to the left by a specified amount
            | SHR Int      -- ^ Pop the top number and push it back, with its
                           --   bits shifted to the right by a specified amount
            | CNT          -- ^ Push the number of one bits in the top number

            -- Branching instructions
            | JMP Addr     -- ^ Jump to the indicated instruction address
            | JMS          -- ^ Jump to the address indicated on top of the
                           --   stack
            | JZE Addr     -- ^ Jump if the top value on the stack is 0
            | JNZ Addr     -- ^ Jump if the top value on the stack is not 0
            | JEQ Addr     -- ^ Jump if the top value on the stack is equal to
                           --   the value beneath it
            | JNQ Addr     -- ^ Jump if the top value on the stack is not equal
                           --   to the value beneath it
            | JGT Addr     -- ^ Jump if the top value on the stack is greater
                           --   than the value beneath it
            | JLT Addr     -- ^ Jump if the top value on the stack is lesser 
                           --   than the value beneath it
            | JQV Int Addr -- ^ Jump if the top value on the stack is equal to
                           --   the given number
            | JNV Int Addr -- ^ Jump if the top value on the stack is not equal
                           --   to the given number
            | JGV Int Addr -- ^ Jump if the top value on the stack is greater
                           --   than the given number
            | JLV Int Addr -- ^ Jump if the top value on the stack is lesser
                           --   than the given number
            deriving (Show, Eq)

-- | Represents instruction addresses.
type Addr = Int

-- | A list of instructions to be executed, implemented as a sequence.
type InstructionList = S.Seq OpCode

-- | Represents required stack sizes for certain operations.
data NeededSize = Zero
                | One
                | Two

-- | Marking opcodes that require a certain stack size to run properly.
{-# INLINE itemsNeededInStack #-}
itemsNeededInStack :: OpCode -> NeededSize
itemsNeededInStack op = case op of
    NOP     -> Zero
    END     -> Zero
    ILL     -> Zero
    PSH _   -> Zero
    JMP _   -> Zero
    POP     -> One
    DUP     -> One
    NOT     -> One
    SHL _   -> One
    SHR _   -> One
    CNT     -> One
    JMS     -> One
    JZE _   -> One
    JNZ _   -> One
    JQV _ _ -> One
    JNV _ _ -> One
    JGV _ _ -> One
    JLV _ _ -> One
    ADD     -> Two
    SUB     -> Two
    MUL     -> Two
    DIV     -> Two
    MOD     -> Two
    AND     -> Two
    IOR     -> Two
    XOR     -> Two
    JEQ _   -> Two
    JNQ _   -> Two
    JGT _   -> Two
    JLT _   -> Two

-- | Shows the program state at each point of execution.
data CPU = CPU { stack :: [Int]  -- ^ Program data stack
               , state :: T.Text -- ^ Log of the current instruction
               , ip    :: Int    -- ^ Instruction pointer
               }
           deriving (Show, Eq)

-- | Pattern matching on stack contents, usually for instructions that require
--   a certain number of values in the stack.
{-# INLINE EmptyStack #-}
pattern EmptyStack :: CPU
pattern EmptyStack <- CPU { stack = [] }
{-# INLINE OneInStack #-}
pattern OneInStack :: CPU
pattern OneInStack <- CPU { stack = (_:[]) }
{-# INLINE ZeroOnTop #-}
pattern ZeroOnTop  :: CPU
pattern ZeroOnTop  <- CPU { stack = (0:_) }

-- | Used at the start of program execution.
emptyCPU :: CPU
emptyCPU = CPU { stack = []
               , state = ""
               , ip    = 0 }

-- | This executes the code and returns a history of all taken actions.
execute :: InstructionList -> [CPU]
execute = runWith emptyCPU

-- | Executes each instruction step by step. It first checks if there is an
--   instruction at the specified address. If not, the program terminates.
runWith :: CPU -> InstructionList -> [CPU]
runWith _ S.Empty = [emptyCPU { state = "No instruction found" }]

runWith cpu code = case S.lookup (ip cpu) code of
    Nothing -> [cpu { state = "No instruction found" }]
    Just op -> checkStack op (itemsNeededInStack op)

    where
    -- | Checks if there are enough items in the stack for certain operations.
    --   If not, the program terminates.
    {-# INLINE checkStack #-}
    checkStack :: OpCode -> NeededSize -> [CPU]
    checkStack op Zero = runOp op

    checkStack op One = case cpu of
        EmptyStack -> [cpu { state = "Not enough values" }]
        _          -> runOp op

    checkStack op Two = case cpu of
        EmptyStack -> [cpu { state = "Not enough values" }]
        OneInStack -> [cpu { state = "Not enough values" }]
        _          -> runOp op

    -- | Performs the instruction. Depending on the kind, it will call utility
    --   functions.
    {-# INLINE runOp #-}
    runOp :: OpCode -> [CPU]
    runOp op = case op of
        NOP       -> let res = cpu { state = "Doing nothing" }
                     in res : runWith res { ip = ip cpu + 1 } code

        END       -> [cpu { state = "Terminated" }]
        ILL       -> [cpu { state = "Unknown instruction" }]

        PSH x     -> push x "value"
        DUP       -> push ((head . stack) cpu) "duplicate"

        POP       -> let res = cpu { stack = (tail . stack) cpu
                                   , state = "Popped value" }
                     in res : runWith res { ip = ip cpu + 1 } code

        ADD       -> calc (+) "Added"
        SUB       -> calc (-) "Subtracted"
        MUL       -> calc (*) "Multiplied"
        DIV       -> calcCheck div "Divided"
        MOD       -> calcCheck mod "Remainder of"

        AND       -> calc (B..&.) "AND of"
        IOR       -> calc (B..|.) "Inclusive OR of"
        XOR       -> calc B.xor "Exclusive OR of"

        NOT       -> let res = cpu { stack = (B.complement . head . stack) cpu :
                                             (tail . stack) cpu
                                   , state = "Complement of the top value" }
                     in res : runWith res { ip = ip cpu + 1 } code
        SHL n     -> shiftCalc (n)
        SHR n     -> shiftCalc (-n)
        CNT       -> push ((B.popCount . head . stack) cpu) "count of ones"

        JMP adr   -> branch adr True
        JMS       -> branch ((head . stack) cpu) True
        JZE adr   -> branch adr $ (head . stack) cpu == 0
        JNZ adr   -> branch adr $ (head . stack) cpu /= 0
        JEQ adr   -> branch adr $ (head . stack) cpu == 
                                  (head . tail . stack) cpu
        JNQ adr   -> branch adr $ (head . stack) cpu /=
                                  (head . tail . stack) cpu
        JGT adr   -> branch adr $ (head . stack) cpu >
                                  (head . tail . stack) cpu
        JLT adr   -> branch adr $ (head . stack) cpu <
                                  (head . tail . stack) cpu
        JQV n adr -> branch adr $ (head . stack) cpu == n
        JNV n adr -> branch adr $ (head . stack) cpu /= n
        JGV n adr -> branch adr $ (head . stack) cpu > n
        JLV n adr -> branch adr $ (head . stack) cpu < n
    
    -- | Pushes values onto the stack.
    {-# INLINE push #-}
    push :: Int -> T.Text -> [CPU]
    push x t = let res = cpu { stack = x : stack cpu, state = "Pushed " <> t }
               in res : runWith res { ip = ip cpu + 1 } code

    -- | Performs numeric operations.
    {-# INLINE calc #-}
    calc :: (Int -> Int -> Int) -> T.Text -> [CPU]
    calc f str = let a   = (head . stack) cpu
                     b   = (tail . stack) cpu
                     res = cpu { stack = (head b `f` a) : tail b
                               , state = str <> " two values" }
                 in res : runWith res { ip = ip cpu + 1 } code

    -- | Checks division by 0 for DIV and MOD.
    {-# INLINE calcCheck #-}
    calcCheck :: (Int -> Int -> Int) -> T.Text -> [CPU]
    calcCheck f str = case cpu of
        ZeroOnTop -> [cpu { state = "Top value is 0" }]
        _         -> calc f str

    -- | Computes the bit shift
    {-# INLINE shiftCalc #-}
    shiftCalc :: Int -> [CPU]
    shiftCalc n = let a   = (head . stack) cpu
                      res = cpu { stack = (B.shift a n) : (tail . stack) cpu
                                , state = "Shifted value" }
                  in res : runWith res { ip = ip cpu + 1 } code
    
    -- | Checks if branching should occur.
    {-# INLINE branch #-}
    branch :: Addr -> Bool -> [CPU]
    branch adr True = let res = cpu { state = "Jumping to specified location" }
                      in res : runWith res { ip = adr } code
    branch _ False  = let res = cpu { state = "Not jumping" }
                      in res : runWith res { ip = ip cpu + 1 } code

