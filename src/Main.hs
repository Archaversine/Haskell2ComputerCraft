{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 
    forFromTo "x" (1.0, 2.0) $ \x -> do 
        forFromTo "y" (1.0, 2.0) $ \y -> do 
            tPrint "PAIR"
            tPrint x 
            tPrint y

            forBreak

main :: IO ()
main = writeProgram prog "prog.lua"
