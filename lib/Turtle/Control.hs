module Turtle.Control ( tWhile
                      , tFunc
                      , tFuncBool 
                      , tFuncBoolE
                      , lFunc
                      ) where 

import Control.Monad
import Control.Monad.Writer

import Data.List (intercalate)

import Turtle.Types

tWhile :: Turtle (TVal Bool) -> Turtle a -> Turtle () 
tWhile cond code = do 
    tell "while " >> cond >> tell "do\n"
    void code
    tell "end\n"

-- Turtle Function
tFunc :: String -> [String] -> Turtle () 
tFunc name = lFunc ("turtle." <> name)

-- Returns True just to satisfy the type checker
tFuncBool :: String -> [String] -> Turtle (TVal Bool)
tFuncBool name params = tFunc name params >> return (TBool "true")

tFuncBoolE :: String -> Turtle (TVal Bool)
tFuncBoolE name = tFuncBool name []

-- Lua Function
lFunc :: String -> [String] -> Turtle ()
lFunc name params = tell $ name <> "(" <> params' <> ")\n"
    where params' = intercalate ", " params
