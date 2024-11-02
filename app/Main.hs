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
    print collatz

    let hist = execute $ fromList collatz
    mapM_ print hist



