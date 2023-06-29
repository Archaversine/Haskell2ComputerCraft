{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 

    x <- defineLocal "x" 1.0

    tIf (x .== 1.0) $ do 
        tPrint "Hello, World!"

main :: IO ()
main = writeProgram prog "prog.lua"
