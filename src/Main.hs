{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 
    refuel ()
    refuel 1.0

    first   <- defineLocal "first" "Hello, "
    second  <- defineLocal "second" "World"
    message <- defineLocal "message" $ first ... second ... "!"

    tPrint message

main :: IO ()
main = writeProgram prog "prog.lua"
