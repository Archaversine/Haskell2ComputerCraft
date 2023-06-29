{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 
    refuel () 

    -- Define a local variable x
    x <- defineLocal "x" 0.0

    -- Define a function that digs after a certain action
    let digAfter a = a >> digM ()

    -- Dig while block is detected under
    tWhile detectDown $ do 
        -- Increment x
        -- Custom operator defined in Turtle.Var
        x += 1.0

        -- Dig Actions
        digM () >> digDownM () >> digAfter moveDownM
        moveForwardM >> turnLeftM

    -- Print the value of x
    tPrint x

main :: IO ()
main = writeProgram prog "prog.lua"
