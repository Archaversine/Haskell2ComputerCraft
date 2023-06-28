module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 
    x <- declareLocal "x"
    y <- defineLocal "y" False

    setVar x (99 :: Float)

    z <- defineLocal "z" $ (1 :: Float) .+ (1 :: Float) .* x

    tPrint x 
    tPrint y
    tPrint z

main :: IO ()
main = writeProgram prog "prog.lua"
