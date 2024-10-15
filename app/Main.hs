module Main (main) where

import Cascade.Assembler (OpCode (..), assembleByteCode)
import Cascade.Runtime (interpretByteCode)

main :: IO ()
main = do
  putStrLn "Cascades, on version 0.0.1"
  putStrLn "Testing at the moment.\n"

  let code = [ (PUSH, [10])
             , (SUB, [1])
             , (POP, [])
             , (ILL, [])
             , (HALT, [])
             ]

  let code2 = [ (PUSH, [5])
              , (NOP, [])
              , (NOP, [])
              , (PUSH, [2])
              , (ADD, [])
              , (PUSH, [10])
              , (SUB, [])
              , (NOP, [])
              , (POP, [])
              , (POP, [])
              , (HALT, [])
              ]

  let asm = assembleByteCode code
  let asm2 = assembleByteCode code2

  putStrLn "Assembly code:"
  print code
  print code2
  putStrLn "Result:"
  print asm
  print asm2

  putStrLn "Running each code"
  let Right x = asm2
  
  mapM_ print (interpretByteCode x)


