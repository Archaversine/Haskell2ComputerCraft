{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 
    tPrint $ 1.0 ... True ... "hi there!" ... "how are you?"

main :: IO ()
main = writeProgram prog "prog.lua"
