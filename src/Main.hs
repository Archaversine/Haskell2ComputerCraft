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
    setCursorPos monitor (1, 1)
    write monitor "Departing!"

    refuel ()

    loopM 10 moveForwardM

    clear term
    setCursorPos term (1, 1)

    blocks <- defineLocal "blocks" 0

    tWhile True $ do 
        digM ()
        blocks += 1
        moveForwardM

        tPrint $ "Mined " ... blocks ... " blocks!"

main :: IO ()
main = do 
    writeProgram prog "prog.lua"

    putStrLn "Generated Code"
    putStrLn "============================"
    printProgram prog
    putStrLn "============================"
