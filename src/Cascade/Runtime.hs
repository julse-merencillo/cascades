-- | This module contains Cascade's runtime.
module Cascade.Runtime
(
    OpCode(..),
    Program,
    runProgram
) where

import qualified Data.Sequence as S
import qualified Data.Bits     as B

-- ( Data Declarations ) ------------------------------------------------------

-- | Represents instruction addresses.
type Addr = Int

-- | Currently available instructions.
data OpCode = NOP       -- ^ Do nothing
            | END       -- ^ Terminate the program
            | ILL       -- ^ Intentionally illegal opcode for testing purposes
            -- Stack instructions
            | PSH Int   -- ^ Store a value onto the stack
            | POP       -- ^ Remove the top value from the stack
            | DUP       -- ^ Duplicate the top value in the stack
            -- Register and memory instructions
            | LVR Int   -- ^ Load a value to the register
            | LMR Addr  -- ^ Load a value from memory to the register
            | SMR Addr  -- ^ Store the value from the register to memory
            -- Stack and register instructions
            | RGS       -- ^ Move the value from the register to the stack
            | SRG       -- ^ Move the top value from the stack to the register
            -- Arithmetic instructions
            | ADD       -- ^ Compute the sum of the top two values
            | SUB       -- ^ Compute the difference of the top two values
            | MUL       -- ^ Compute the product of the top two values
            | DIV       -- ^ Compute the quotient of the top two values
            | MOD       -- ^ Compute the modulo of the top two valuess
            -- Bitwise instructions
            | AND       -- ^ Compute the logical AND of the top two values
            | IOR       -- ^ Compute the logical OR of the top two values
            | XOR       -- ^ Compute the logical XOR of the top two values
            | NOT       -- ^ Compute the ones' complement of the top value
            | SHL Int   -- ^ Shift the bits of the top value to the left
            | SHR Int   -- ^ Shift the bits of the top value to the right
            | CNT       -- ^ Push the number of one bits in the top value
            -- Branching instructions
            | JMP Addr  -- ^ Jump to the specified address
            | JMS       -- ^ Jump to the specified address on top of the stack
            | JMR       -- ^ Jump to the specified address on the register
            | JZE Addr  -- ^ Jump if the top value is 0
            | JNZ Addr  -- ^ Jump if the top value is not 0
            | JEQ Addr  -- ^ Jump if the top value and register are equal
            | JNQ Addr  -- ^ Jump if the top value and register are not equal
            | JGT Addr  -- ^ Jump if the top value is greater than the register
            | JLT Addr  -- ^ Jump if the top value is lesser than the register
            deriving (Show)

-- | A list of instructions to be executed by the machine.
type Program = S.Seq OpCode

-- | Contains program state during execution.
data ExecUnit = PState { ip    :: Int    -- ^ Instruction pointer
                       , reg   :: Int    -- ^ Program register
                       , instr :: OpCode -- ^ Current instruction being run
                       , stack :: [Int]  -- ^ Program stack
                       }
                deriving (Show)

-- | Helper function to access the head of the stack in a given state
first :: ExecUnit -> Int
{-# INLINE first #-}
first = (head . stack)

-- | Helper function to access the tail of the stack in a given state
rest :: ExecUnit -> [Int]
{-# INLINE rest #-}
rest = (tail . stack)

-- | Possible errors that could occur during runtime
data RuntimeError = DivByZero      -- ^ Division by zero
                  | NotEnoughItems -- ^ Not enough items in the stack
                  | IntentionalEnd -- ^ Attempting to run an ILL instruction
                  deriving (Show)

-- | Indicates the status of the current program. 
data Status = Success | Failure RuntimeError | ProgramEnd

-- ( Program Execution ) ------------------------------------------------------

-- | Initial program state before execution.
initState :: ExecUnit
{-# INLINE initState #-}
initState = PState { stack = []
                   , ip    = 0
                   , reg   = 0
                   , instr = NOP }

-- | Runs the program in a fetch-decode-execute cycle.
runProgram :: Program -> IO ()
runProgram S.Empty = error "Nothing to run."
runProgram prog    = fetch prog initState

-- | Fetches the instruction pointed to by the instruction pointer. If none
--   exists, the program halts.
fetch :: Program -> ExecUnit -> IO ()
{-# INLINE fetch #-}
fetch prog state = do
    case S.lookup (ip state) prog of
        Nothing -> error "No instruction found."
        Just op -> decode prog $ state { instr = op }

-- | Prepares for execution by checking if the program state won't cause any
--   errors. If there are, the program halts.
decode :: Program -> ExecUnit -> IO ()
{-# INLINE decode #-}
decode prog state = do
    case check state of
        Success       -> execute prog state
        Failure cause -> error $ "Cannot run instruction " ++ 
                         show (instr state) ++ " due to " ++ show cause
        ProgramEnd    -> putStrLn ""
    
-- | Executes the instruction.
execute :: Program -> ExecUnit -> IO ()
{-# INLINE execute #-}
execute prog state = do
    newState <- runOp state
    print newState
    fetch prog newState
    
-- ( Error Checking ) ---------------------------------------------------------

-- | Checks the current state and instruction to run and determines if the
--   program can keep running.
check :: ExecUnit -> Status
{-# INLINE check #-}
check state = case (instr state, stack state) of
    -- Division by 0: The top of the stack is 0
    (DIV  , (0:_))  -> Failure DivByZero
    (MOD  , (0:_))  -> Failure DivByZero
    -- Operations that require an item in the stack: The stack is empty
    (POP  , [])     -> Failure NotEnoughItems
    (DUP  , [])     -> Failure NotEnoughItems
    (SRG  , [])     -> Failure NotEnoughItems
    (ADD  , [])     -> Failure NotEnoughItems
    (SUB  , [])     -> Failure NotEnoughItems
    (MUL  , [])     -> Failure NotEnoughItems
    (DIV  , [])     -> Failure NotEnoughItems
    (MOD  , [])     -> Failure NotEnoughItems
    (AND  , [])     -> Failure NotEnoughItems
    (IOR  , [])     -> Failure NotEnoughItems
    (NOT  , [])     -> Failure NotEnoughItems
    (SHL _, [])     -> Failure NotEnoughItems
    (SHR _, [])     -> Failure NotEnoughItems
    (CNT  , [])     -> Failure NotEnoughItems
    (JMS  , [])     -> Failure NotEnoughItems
    (JZE _, [])     -> Failure NotEnoughItems
    (JNZ _, [])     -> Failure NotEnoughItems
    (JEQ _, [])     -> Failure NotEnoughItems
    (JNQ _, [])     -> Failure NotEnoughItems
    (JGT _, [])     -> Failure NotEnoughItems
    (JLT _, [])     -> Failure NotEnoughItems
    -- Operations that require two items in the stack: There is only one item
    (ADD  , (_:[])) -> Failure NotEnoughItems
    (SUB  , (_:[])) -> Failure NotEnoughItems
    (MUL  , (_:[])) -> Failure NotEnoughItems
    (DIV  , (_:[])) -> Failure NotEnoughItems
    (MOD  , (_:[])) -> Failure NotEnoughItems
    (AND  , (_:[])) -> Failure NotEnoughItems
    (IOR  , (_:[])) -> Failure NotEnoughItems
    (XOR  , (_:[])) -> Failure NotEnoughItems
    -- Program termination
    (END  , _)      -> ProgramEnd
    (ILL  , _)      -> Failure IntentionalEnd
    -- Checked all failure/termination cases past this point
    (_,_) -> Success

-- ( Instruction Execution ) --------------------------------------------------

-- | Performs the actual computation and returns the state after execution.
runOp :: ExecUnit -> IO ExecUnit
{-# INLINE runOp #-}
runOp state = return $ case instr state of
    -- Stack instructions
    PSH num -> push state num
    POP     -> state { stack = rest state, ip = ip state + 1 }
    DUP     -> push state $ first state
    -- Register-memory instructions
    LVR num -> state { reg = num, ip = ip state + 1 }
    {-LMR adr-}
    {-SMR adr-}
    -- Stack-register instructions
    RGS     -> move state toStack
    SRG     -> move state toRegister
    -- Arithmetic instructions
    ADD     -> calc state (+)
    SUB     -> calc state (-)
    MUL     -> calc state (*)
    DIV     -> calc state div
    MOD     -> calc state mod
    -- Bitwise instructions
    AND     -> calc state (B..&.)
    IOR     -> calc state (B..|.)
    XOR     -> calc state B.xor
    NOT     -> state { stack = (B.complement . first) state : rest state
                     , ip    = ip state + 1 }
    SHL num -> calcShift state num
    SHR num -> calcShift state (-num)
    CNT     -> push state $ (B.popCount . first) state
    -- Branching instructions
    JMP adr -> jump state adr           True
    JMS     -> jump state (first state) True
    JMR     -> jump state (reg state)   True
    JZE adr -> jump state adr $ (first state) == 0
    JNZ adr -> jump state adr $ (first state) /= 0
    JEQ adr -> jump state adr $ (first state) == (reg state) 
    JNQ adr -> jump state adr $ (first state) /= (reg state)
    JGT adr -> jump state adr $ (first state) > (reg state)
    JLT adr -> jump state adr $ (first state) < (reg state)
    -- Unimplemented instructions are treated as NOPs
    _   -> state
    where
    toStack    = True
    toRegister = False

-- | Pushes a number to the stack
push :: ExecUnit -> Int -> ExecUnit
{-# INLINE push #-}
push state num = state { stack = num : stack state, ip = ip state + 1 }

-- | Performs an arithmetic or bitwise calculation
calc :: ExecUnit -> (Int -> Int -> Int) -> ExecUnit
{-# INLINE calc #-}
calc state f = let a = first state
                   b = rest state
               in state { stack = (head b `f` a) : tail b, ip = ip state + 1}

-- | Performs a bit shift operation
calcShift :: ExecUnit -> Int -> ExecUnit
{-# INLINE calcShift #-}
calcShift state num = let a = first state
                      in state { stack = (B.shift a num) : rest state
                               , ip    = ip state + 1 }

-- | Performs a branch instruction
jump :: ExecUnit -> Addr -> Bool -> ExecUnit
{-# INLINE jump #-}
jump state adr True  = state { ip = adr }
jump state _   False = state { ip = ip state + 1 }

-- | Moves values between the register and the stack
move :: ExecUnit -> Bool -> ExecUnit
{-# INLINE move #-}
move state True  = state { reg = 0, stack = reg state : stack state, 
                           ip  = ip state + 1 }
move state False = state { reg = first state, stack = rest state,
                           ip  = ip state + 1 }
