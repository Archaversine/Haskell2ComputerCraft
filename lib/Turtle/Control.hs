{-# LANGUAGE FlexibleContexts #-}

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

tWhile :: ToTVal a Bool => Turtle a -> Turtle b -> Turtle () 
tWhile cond code = do 
    condVal <- tStr . toTVal <$> cond

    tell "while " >> tell condVal >> tell " do\n"
    void code
    tell "end\n"

-- Turtle Function
tFunc :: String -> [String] -> Turtle () 
tFunc name = lFunc ("turtle." <> name)

tFuncE :: String -> Turtle ()
tFuncE = flip tFunc []

tFuncBool :: String -> [String] -> Turtle (TVal Bool)
tFuncBool name params = tFunc name params >> return (TBool $ tfName name params)

tFuncBoolE :: String -> Turtle (TVal Bool)
tFuncBoolE = flip tFuncBool []

-- Lua Function
lFunc :: String -> [String] -> Turtle ()
lFunc name params = tell $ fName name params <> "\n"

-- Function name 
fName :: String -> [String] -> String
fName name params = name <> "(" <> intercalate ", " params <> ")"

tfName :: String -> [String] -> String 
tfName name = fName $ "turtle." <> name
