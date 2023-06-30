{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Turtle.Var ( declareLocal
                  , defineLocal
                  , setVar
                  , (+=), (-=), (*=), (//=)
                  ) where

import Control.Monad.Writer

import Turtle.Types

-- | Declare a local variable 
declareLocal :: String -> Turtle (TVal TurtleVar)
declareLocal name = do 
    tell $ "local " <> name <> "\n"
    return $ TTVar name

-- | Define a local variable
defineLocal :: ToTVal a b => String -> a -> Turtle (TVal TurtleVar)
defineLocal name value = do 
    tell $ "local " <> name <> " = " <> showTVal value <> "\n"
    return $ TTVar name

-- | Modify the value of a local variable
setVar :: ToTVal a b => TVal TurtleVar -> a -> Turtle ()
setVar (TTVar name) value = tell $ name <> " = " <> showTVal value <> "\n"

(+=) :: NumericTVal num a' => TVal TurtleVar -> num -> Turtle ()
x += value = setVar x $ x .+ value

(-=) :: NumericTVal num a' => TVal TurtleVar -> num -> Turtle ()
x -= value = setVar x $ x .- value

(*=) :: NumericTVal num a' => TVal TurtleVar -> num -> Turtle ()
x *= value = setVar x $ x .* value

-- Using //= instead of /= to not conflict with Prelude./=
(//=) :: NumericTVal num a' => TVal TurtleVar -> num -> Turtle ()
x //= value = setVar x $ x ./ value

infixr 0 += 
infixr 0 -= 
infixr 0 *= 
infixr 0 //= 
