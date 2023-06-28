{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 
    first   <- defineLocal "first" "Hello, "
    second  <- defineLocal "second" "World"
    message <- defineLocal "message" $ first ... second ... "!"

    tPrint message

main :: IO ()
main = writeProgram prog "prog.lua"
