{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 

    tIf True $ do 
        tPrint "Hello, World!"

    tIfElse True $ do 
        tPrint "Hello, World!"
    tElseIf False $ do 
        tPrint "Goodbye, World!"
    tElse $ do 
        tPrint "Something happened"

main :: IO ()
main = writeProgram prog "prog.lua"
