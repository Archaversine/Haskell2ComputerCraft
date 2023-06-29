{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 
    tPrint $ 1.0 .> 2.0
    tPrint $ 1.0 .< 2.0
    tPrint $ 1.0 .>= 2.0
    tPrint $ 1.0 .<= 2.0

main :: IO ()
main = writeProgram prog "prog.lua"
