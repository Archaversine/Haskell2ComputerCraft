{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Turtle.Control ( tWhile
                      , tIf
                      , tIfElse
                      , tElseIf
                      , tElseIfEnd
                      , tElse
                      , toParams
                      , turtle
                      , callTFunc
                      , callTFuncE
                      , tFuncBool 
                      , tFuncBoolE
                      , callFunc
                      ) where 

import Control.Monad
import Control.Monad.Writer

import Data.List (intercalate)

import Turtle.Types

tWhile :: TruthyTVal a a' => a -> Turtle _ -> Turtle () 
tWhile cond code = do 
    let condVal = tStr $ toTVal cond

    tell "while " >> tell condVal >> tell " do\n"
    void code
    tell "end\n"

tIf :: TruthyTVal a a' => a -> Turtle _ -> Turtle ()
tIf cond code = do 
    let condVal = tStr $ toTVal cond

    tell "if " >> tell condVal >> tell " then\n"
    void code
    tell "end\n"

tIfElse :: TruthyTVal a a' => a -> Turtle _ -> Turtle ()
tIfElse cond code = do 
    let condVal = tStr $ toTVal cond
    tell "if " >> tell condVal >> tell " then\n" >> void code

tElseIf :: TruthyTVal a a' => a -> Turtle _ -> Turtle ()
tElseIf cond code = do 
    let condVal = tStr $ toTVal cond
    tell "elseif " >> tell condVal >> tell " then\n" >> void code

tElseIfEnd :: TruthyTVal a a' => a -> Turtle _ -> Turtle ()
tElseIfEnd cond code = do 
    let condVal = tStr $ toTVal cond 
    tell "elseif " >> tell condVal >> tell " then\n"
    void code
    tell "end\n"

tElse :: Turtle _ -> Turtle ()
tElse code = do 
    tell "else\n"
    void code
    tell "end\n"

toParams :: [String] -> String
toParams xs = "(" <> intercalate ", " xs <> ")"

-- Turtle Function
turtle :: TVal a -> Turtle ()
turtle f = tell $ tStr f <> "\n"

callTFunc :: String -> [String] -> Turtle () 
callTFunc name = callFunc ("turtle." <> name)

callTFuncE :: String -> Turtle ()
callTFuncE = flip callTFunc []

tFuncBool :: String -> [String] -> TVal Bool
tFuncBool name params = TBool $ tfName name params

tFuncBoolE :: String -> TVal Bool
tFuncBoolE = flip tFuncBool []

-- Lua Function
callFunc :: String -> [String] -> Turtle ()
callFunc name params = tell $ fName name params <> "\n"

-- Function name 
fName :: String -> [String] -> String
fName name params = name <> "(" <> intercalate ", " params <> ")"

tfName :: String -> [String] -> String 
tfName name = fName $ "turtle." <> name
