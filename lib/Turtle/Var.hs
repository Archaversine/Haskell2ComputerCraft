{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Turtle.Var ( declareLocal
                  , defineLocal
                  , setVar
                  , (+=), (-=), (*=), (//=)
                  ) where

import Control.Monad.Writer

import Turtle.Types

declareLocal :: String -> Turtle (TVal TurtleVar)
declareLocal name = do 
    tell $ "local " <> name <> "\n"
    return $ TTVar name

defineLocal :: ToTVal a b => String -> a -> Turtle (TVal TurtleVar)
defineLocal name value = do 
    tell $ "local " <> name <> " = " <> showTVal value <> "\n"
    return $ TTVar name

setVar :: ToTVal a b => TVal TurtleVar -> a -> Turtle ()
setVar (TTVar name) value = tell $ name <> " = " <> showTVal value <> "\n"

(+=) :: NumericTVal a a' => TVal TurtleVar -> a -> Turtle ()
x += value = setVar x $ x .+ value

(-=) :: NumericTVal a a' => TVal TurtleVar -> a -> Turtle ()
x -= value = setVar x $ x .- value

(*=) :: NumericTVal a a' => TVal TurtleVar -> a -> Turtle ()
x *= value = setVar x $ x .* value

-- Using //= instead of /= to not conflict with Prelude./=
(//=) :: NumericTVal a a' => TVal TurtleVar -> a -> Turtle ()
x //= value = setVar x $ x ./ value

infixr 0 += 
infixr 0 -= 
infixr 0 *= 
infixr 0 //= 
