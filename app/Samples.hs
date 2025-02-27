-- | This module contains sample Cascade programs.
module Samples
(
    collatz,
    empty,
    infiloop,
    zerodiv,
    swaptest,
    memtest
) where

import Cascade.Runtime
import qualified Data.Sequence as S (fromList)

-- | Computes the Collatz sequence. The next number in the sequence is computed
--   with this rule: if the number is even, divide by two; otherwise, multiply
--   by 3 and add 1.
--
--   If we apply this rule repeatedly for positive integers, it is conjectured 
--   to always end up in a 4-2-1 loop. This program terminates when we reach
--   the number 1.
collatz :: Program
collatz = S.fromList [ LVR 1       -- Load 1 to the register for comparison
                     , PSH 1023456 -- Push the initial number
                     , DUP         -- Use a duplicate to check divisibility
                     , PSH 2       -- Check divisibility by 2 (parity)
                     , MOD
                     , JNZ 10      -- Jump to odd case (nondivisible by 2)
                     , POP         -- Remove the duplicate, even case
                     , SHR 1       -- Divide by 2
                     , JNQ 2       -- If not 1, return to DUP
                     , END         -- If it was 1, end the program
                     , POP         -- Remove the duplicate, odd case
                     , PSH 3       -- Multiply by 3
                     , MUL
                     , PSH 1       -- Add 1
                     , ADD
                     , JMP 2       -- Jump back to DUP
                     ]

-- | An empty program. Used only for testing purposes.
empty :: Program
empty = S.fromList []

-- | A program that infinitely loops and fills up the stack.
infiloop :: Program
infiloop = S.fromList [ PSH 1 -- Push a number to the stack (we chose 1)
                      , JMP 0 -- Repeat the first instruction
                      ]

-- | Division by zero test
zerodiv :: Program
zerodiv = S.fromList [ PSH 3
                     , PSH 0
                     , DIV
                     , END
                     ]

-- | Swap test
swaptest :: Program
swaptest = S.fromList [ PSH 1
                      , LVR 10
                      , SWP
                      , END
                      ]
-- | Memory test
memtest :: Program
memtest = S.fromList [ LVR 12
                     , PNT 9
                     , STR
                     , LVR 1
                     , PNT 0
                     , STR
                     , PNT 9
                     , LDR
                     , END
                     ]

