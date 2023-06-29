{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import Turtle

add :: TVal TurtleVar -> TVal TurtleVar -> TVal Double
add a b = a .+ b

prog :: Turtle ()
prog = do 
    x <- defineLocal "x" 1.0
    y <- defineLocal "y" 1.0

    z <- defineLocal "z" (add x y)

    tPrint z

main :: IO ()
main = writeProgram prog "prog.lua"
