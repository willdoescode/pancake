module Main where

import Expr

main :: IO ()
main = putStrLn $ eval (PFunc (PAddition (PType (PString "Hello")) (PType (PString " World"))))
