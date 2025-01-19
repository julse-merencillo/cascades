module Main 
(
    main
) where

import Cascade.Runtime
import Samples

versionNumber :: String
versionNumber = "0.2.1"

main :: IO ()
main = do
    putStrLn $ "Cascades, on version " ++ versionNumber 
    runProgram regToStackAndBack


