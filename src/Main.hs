{-# LANGUAGE ExtendedDefaultRules #-}

--{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 
    digM ()

    x <- defineLocal "x" $ dig ()

    tPrint x

main :: IO ()
main = writeProgram prog "prog.lua"
