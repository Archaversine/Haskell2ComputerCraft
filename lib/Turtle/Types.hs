{-# LANGUAGE FlexibleInstances #-}

module Turtle.Types ( Turtle
                    , ToolSide(..)
                    , TString(..)
                    , TExpr(..)
                    , leftSide
                    , rightSide
                    , (.+), (.-), (.*), (./)
                    ) where

import Control.Monad.Writer

type Turtle = Writer String

class TString a where 
    tStr :: a -> String

instance TString Bool where 
    tStr True = "true"
    tStr False = "false"

instance TString Int where 
    tStr = show

instance TString Integer where 
    tStr = show

instance TString Float where 
    tStr = show

instance TString Double where 
    tStr = show

instance TString String where 
    tStr = show

data ToolSide = LeftSide | RightSide

instance Show ToolSide where 
    show LeftSide = "left"
    show RightSide = "right"

leftSide :: Maybe ToolSide 
leftSide = Just LeftSide

rightSide :: Maybe ToolSide
rightSide = Just RightSide

data TExpr a = TConst a
             | TAdd (TExpr a) (TExpr a) 
             | TSub (TExpr a) (TExpr a) 
             | TMul (TExpr a) (TExpr a) 
             | TDiv (TExpr a) (TExpr a) 

instance TString a => TString (TExpr a) where 
    tStr = evalTExpr

evalTExpr :: TString a => TExpr a -> String 
evalTExpr (TConst x) = tStr x
evalTExpr (TAdd e1 e2) = "(" <> evalTExpr e1 <> " + " <> evalTExpr e2 <> ")"
evalTExpr (TSub e1 e2) = "(" <> evalTExpr e1 <> " - " <> evalTExpr e2 <> ")"
evalTExpr (TMul e1 e2) = "(" <> evalTExpr e1 <> " * " <> evalTExpr e2 <> ")"
evalTExpr (TDiv e1 e2) = "(" <> evalTExpr e1 <> " / " <> evalTExpr e2 <> ")"

(.+) :: TString a => a -> a -> TExpr a
a .+ b = TAdd (TConst a) (TConst b)

(.-) :: TString a => a -> a -> TExpr a
a .- b = TSub (TConst a) (TConst b)

(.*) :: TString a => a -> a -> TExpr a
a .* b = TMul (TConst a) (TConst b)

(./) :: TString a => a -> a -> TExpr a
a ./ b = TDiv (TConst a) (TConst b)

infixl 6 .+ 
infixl 6 .- 
infixl 7 .*
infixl 7 ./

infixl 6 `TAdd`
infixl 6 `TSub`
infixl 7 `TMul`
infixl 7 `TDiv`
