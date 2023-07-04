{-# LANGUAGE ExtendedDefaultRules #-}

--{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 
    (InspectData name meta) <- inspect ("name", "meta")

    tPrint $ "Block name: " ... name
    tPrint $ "Block meta: " ... meta

main :: IO ()
main = do 
    writeProgram prog "prog.lua"

    putStrLn "Generated Code"
    putStrLn "============================"
    printProgram prog
    putStrLn "============================"
