-- | This module contains sample Cascade programs.
module Samples
(
    collatz,
    empty,
    infiloop,
    regToStackAndBack
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
                     , JNZ 11      -- Jump to odd case (nondivisible by 2)
                     , POP         -- Remove the duplicate, even case
                     , PSH 2       -- Divide by 2
                     , DIV         
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

-- | Test program and all that stuff
regToStackAndBack :: Program
regToStackAndBack = S.fromList [ LVR 10
                               , RGS
                               , LVR 5
                               , RGS
                               , SRG
                               , SRG
                               , END
                               ]




