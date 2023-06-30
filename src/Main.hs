{-# LANGUAGE ExtendedDefaultRules #-}

--{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 
    selectM 1.0

    x <- defineLocal "x" getSelectedSlot

    tPrint x

main :: IO ()
main = writeProgram prog "prog.lua"
