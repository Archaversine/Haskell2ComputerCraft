{-# LANGUAGE ExtendedDefaultRules #-}

--{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 
    term <- wrapTerm
    monitor <- wrapPeripheral "monitor" MonLeft 

    clear monitor 
    setCursorPos monitor (1.0, 1.0)
    write monitor "Departing!"

    refuel ()

    loopM 10.0 moveForwardM

    clear term
    setCursorPos term (1.0, 1.0)

    blocks <- defineLocal "blocks" 0.0

    tWhile True $ do 
        digM ()
        blocks += 1.0
        moveForwardM

        tPrint $ "Mined " ... blocks ... " blocks!"

main :: IO ()
main = do 
    writeProgram prog "prog.lua"

    putStrLn "Generated Code"
    putStrLn "============================"
    printProgram prog
    putStrLn "============================"
