{-# LANGUAGE GADTs #-}

module Turtle.Var ( declareLocal
                  , defineLocal
                  , setVar
                  ) where

import Control.Monad.Writer

import Turtle.Types

declareLocal :: String -> Turtle (TVal TurtleVar)
declareLocal name = do 
    tell $ "local " <> name <> "\n"
    return $ TTVar name

defineLocal :: ToTVal a b => String -> a -> Turtle (TVal TurtleVar)
defineLocal name value = do 
    tell $ "local " <> name <> " = " <> tStr (toTVal value) <> "\n"
    return $ TTVar name

setVar :: ToTVal a b => TVal TurtleVar -> a -> Turtle ()
setVar (TTVar name) value = tell $ name <> " = " <> tStr (toTVal value) <> "\n"
