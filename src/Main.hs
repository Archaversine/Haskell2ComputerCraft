{-# LANGUAGE ExtendedDefaultRules #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Turtle

add a b = a .+ b

prog :: Turtle ()
prog = do 
    x <- defineLocal "x" 5.0

    tIfElse (x % 2.0 .== 0.0) $ do 
        tPrint "X is even!"
    tElse $ do 
        tPrint "X is odd!"

    mySum <- defineLocal "sum" 0.0

    forFromToStep "i" (1.0, 10.0, 1.0) $ \i -> do 
        mySum += i

    tWhile (mySum .> 0.0) $ do 
        mySum -= 1.0
        tPrint "Removed 1!"

    y <- defineLocal "y" 1.0 
    z <- defineLocal "z" 1.0

    result <- defineLocal "result" $ add y z

    tPrint result

main :: IO ()
main = writeProgram prog "prog.lua"
