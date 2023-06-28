module Turtle.Control ( tWhile
                      , tFunc
                      , tFuncE
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

tFuncE :: String -> Turtle ()
tFuncE = flip tFunc []

-- Returns True just to satisfy the type checker
tFuncBool :: String -> [String] -> Turtle (TVal Bool)
tFuncBool name params = tFunc name params >> return (toTVal True)

tFuncBoolE :: String -> Turtle (TVal Bool)
tFuncBoolE = flip tFuncBool []

-- Lua Function
lFunc :: String -> [String] -> Turtle ()
lFunc name params = tell $ name <> "(" <> params' <> ")\n"
    where params' = intercalate ", " params
