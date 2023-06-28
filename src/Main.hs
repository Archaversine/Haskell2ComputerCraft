{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 
    x <- declareLocal "x"
    y <- defineLocal "y" False

    setVar x 99.0

    z <- defineLocal "z" $ 1.0 .+ 1.0 .* x

    tPrint x 
    tPrint y
    tPrint z

main :: IO ()
main = writeProgram prog "prog.lua"
