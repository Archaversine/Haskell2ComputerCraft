{-# LANGUAGE ExtendedDefaultRules #-}

--{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Turtle

prog :: Turtle ()
prog = do 
    (ret, blockData) <- inspectDown ("ret", "blockData")

    tIf ret $ do 
        tPrint $ "Block Name: " ... blockName blockData
        tPrint $ "Block meta: " ... blockMetadata blockData

main :: IO ()
main = do 
    writeProgram prog "prog.lua"

    putStrLn "Generated Code"
    putStrLn "============================"
    printProgram prog
    putStrLn "============================"
