{-# LANGUAGE ExtendedDefaultRules #-}

--{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Turtle

prog :: Turtle ()
prog = tWhile True $ do 
    (ret, blockData) <- inspect ("ret", "blockData")

    tIf (ret `tAnd` (blockName blockData .== "ae2:quartz_cluster")) $ do 
        digM ()

main :: IO ()
main = do 
    writeProgram prog "prog.lua"

    putStrLn "Generated Code"
    putStrLn "============================"
    printProgram prog
    putStrLn "============================"
