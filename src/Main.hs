module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 
    refuel Nothing

    tWhile detectDown $ do 
        dig Nothing 
        digDown Nothing 
        down 
        dig Nothing 
        forward
        turnLeft

main :: IO ()
main = writeProgram prog "prog.lua"
