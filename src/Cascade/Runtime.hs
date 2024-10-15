module Cascade.Runtime
(
  CPU (..),
  interpretByteCode
) where

{- Imports (none) -}

data CPU = CPU { stack      :: [Int]
               , state_log  :: String
               , panic      :: Bool
               }
           deriving (Show, Eq)

emptyCPU :: CPU
emptyCPU = CPU { stack     = []
               , state_log = ""
               , panic     = False
               }

interpretByteCode :: [Int] -> [CPU]
interpretByteCode = interpret [emptyCPU]

interpret :: [CPU] -> [Int] -> [CPU]
interpret [] _ = []
interpret (cpux:_) [] = [cpux {state_log = "End of instruction.", panic = True}] 
interpret cpu@(cpux:_) code@(code1:code2) = 
  case code1 of
  0 -> cpux {state_log = "Doing nothing."} : interpret cpu code2          -- NOP
  1 -> [cpux {state_log = "Terminated.", panic = True}]                   -- HALT
  2 -> push cpu code2                                                     -- PUSH
  3 -> pop cpu code                                                       -- POP
  4 -> add cpu code                                                       -- ADD
  5 -> sub cpu code                                                       -- SUB
  6 -> [cpux {state_log = "Illegal operation.", panic = True}] -- ILL
  _ -> [cpux {state_log = "Instruction not found.", panic = True}]        -- Not found

push :: [CPU] -> [Int] -> [CPU]
push [] _ = [emptyCPU {state_log = "No CPU to run on.", panic = True}]
push _ [] = [emptyCPU {state_log = "End of instruction.", panic = True}]
push cpu@(cpux:_) (code:code2) = 
  let res = cpux { stack = code : stack cpux
                 , state_log = "Pushed value."
                 }
  in res : interpret (res : cpu) code2

pop :: [CPU] -> [Int] -> [CPU]
pop [] _ = [emptyCPU {state_log = "No CPU to run on.", panic = True}]
pop _ [] = [emptyCPU {state_log = "No further instructions.", panic = True}]
pop (CPU {stack = []}:_) _ = [emptyCPU {state_log = "No items on stack.", panic = True}]
pop cpu@(cpux:_) (_:code2) =
  let res = cpux { stack = (tail . stack) cpux
                 , state_log = "Popped value"
                 }
  in res : interpret (res : cpu) code2

add :: [CPU] -> [Int] -> [CPU]
add [] _ = [emptyCPU {state_log = "No CPU to run on.", panic = True}]
add _ [] = [emptyCPU {state_log = "No further instructions.", panic = True}]
add cpu@(cpux:_) (_:code2) = 
  let a = (head . stack) cpux
      b = (tail . stack) cpux
      res = cpux { stack = (head b + a) : tail b
                 , state_log = "Added two values"
                 }
  in res : interpret (res : cpu) code2

sub :: [CPU] -> [Int] -> [CPU]
sub [] _ = [emptyCPU {state_log = "No CPU to run on.", panic = True}]
sub _ [] = [emptyCPU {state_log = "No further instructions.", panic = True}]
sub cpu@(cpux:_) (_:code2) = 
  let a = (head . stack) cpux
      b = (tail . stack) cpux
      res = cpux { stack = (head b - a) : tail b 
                 , state_log = "Subtracted two values"
                 }
  in res : interpret (res : cpu) code2
  

