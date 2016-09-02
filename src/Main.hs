module Main where

import NeedIt.NeedIt (getAdvPLRepo)

main :: IO ()
main = putStrLn "Base repository: " >> getLine >>= getAdvPLRepo
