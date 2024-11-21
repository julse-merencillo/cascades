module Main (main) where

import Cascade.Runtime
import Data.Sequence (fromList)

versionNumber :: String
versionNumber = "0.1.1"

main :: IO ()
main = do
    putStrLn $ "Cascades, on version " ++ versionNumber 
    putStrLn "Testing at the moment\n"

    -- Collatz sequence
    let collatz = [ PSH 3
                  , DUP
                  , PSH 2
                  , MOD
                  , JNZ 10
                  , POP
                  , PSH 2
                  , DIV
                  , JNV 1 1
                  , END
                  , POP
                  , PSH 3
                  , MUL
                  , PSH 1
                  , ADD
                  , JNV 1 1
                  , END
                  ]

    -- Bitwise test
    let bitwise = [ PSH 10
                  , SHL 2
                  , SHR 3
                  , PSH 3
                  , AND
                  , PSH 12
                  , IOR
                  , PSH 1
                  , XOR
                  , CNT
                  , END
                  ]
    print bitwise

    let hist = execute $ fromList bitwise
    mapM_ print hist



