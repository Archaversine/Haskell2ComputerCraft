{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 
    refuel () 

    tWhile (pure True) $ do 
        dig () >> digDown () >> down >> dig () >> forward >> turnLeft

main :: IO ()
main = writeProgram prog "prog.lua"
