{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Turtle.Types ( Turtle
                    , TurtleVar(..)
                    , TVal(..)
                    , ToTVal(..)
                    , NotString
                    , TruthyTVal
                    , NumericTVal
                    , StringyTVal
                    , showTVal
                    , tAdd, tSub, tMul, tDiv
                    , (.+), (.-), (.*), (./)
                    , (...)
                    , (.==), (~=)
                    , (.>), (.<), (.>=), (.<=)
                    ) where

import Control.Monad.Writer

import Data.Kind (Constraint)
import GHC.TypeLits

type Turtle = Writer String

newtype TurtleVar = TurtleVar String

class ToTVal a b | a -> b where 
    toTVal :: a -> TVal b

instance ToTVal Double Double where 
    toTVal = TDouble . show

instance ToTVal Bool Bool where 
    toTVal True = TBool "true"
    toTVal False = TBool "false"

instance ToTVal String String where 
    toTVal = TStr

instance ToTVal TurtleVar TurtleVar where 
    toTVal (TurtleVar name) = TTVar name

instance ToTVal (TVal a) a where 
    toTVal = id

data StrVar

data TVal a where 
    TDouble :: String -> TVal Double
    TBool   :: String -> TVal Bool
    TStr    :: String -> TVal String
    TTVar   :: String -> TVal TurtleVar
    TStrVar :: String -> TVal StrVar

instance Show (TVal a) where 
    show (TDouble x) = x
    show (TBool x)   = x
    show (TStr x)    = show x
    show (TTVar x)   = x
    show (TStrVar x) = x

showTVal :: ToTVal a a' => a -> String
showTVal = show . toTVal

type family NotString (a :: *) :: Constraint where 
    NotString String = TypeError ('Text "Type cannot be a String or StrVar!")
    NotString StrVar = TypeError ('Text "Type cannot be a String or StrVar!")
    NotString _      = ()

type family TNum (a :: *) :: Constraint where 
    TNum Double    = () 
    TNum TurtleVar = () 
    TNum _         = TypeError ('Text "Type must be a TNum type")

type family NumericTVal (a :: *) (b :: *) :: Constraint where 
    NumericTVal a b = (ToTVal a b, TNum b)

applyTOp :: (ToTVal a a', ToTVal b b') => String -> a -> b -> String
applyTOp op a b = "(" <> showTVal a <> " " <> op <> " " <> showTVal b <> ")"

tAdd :: (NumericTVal a a', NumericTVal b b') => a -> b -> TVal Double 
tAdd a b = TDouble $ applyTOp "+" a b

tSub :: (NumericTVal a a', NumericTVal b b') => a -> b -> TVal Double 
tSub a b = TDouble $ applyTOp "-" a b

tMul :: (NumericTVal a a', NumericTVal b b') => a -> b -> TVal Double 
tMul a b = TDouble $ applyTOp "*" a b

tDiv :: (NumericTVal a a', NumericTVal b b') => a -> b -> TVal Double 
tDiv a b = TDouble $ applyTOp "/" a b

(.+) :: (NumericTVal a a', NumericTVal b b') => a -> b -> TVal Double
(.+) = tAdd

(.-) :: (NumericTVal a a', NumericTVal b b') => a -> b -> TVal Double
(.-) = tSub

(.*) :: (NumericTVal a a', NumericTVal b b') => a -> b -> TVal Double
(.*) = tMul

(./) :: (NumericTVal a a', NumericTVal b b') => a -> b -> TVal Double
(./) = tDiv

infixl 6 .+
infixl 6 .-
infixl 7 .*
infixl 7 ./

-- Types that can be used as strings
type family TValStringable (a :: *) :: Constraint where 
    TValStringable String    = ()
    TValStringable TurtleVar = ()
    TValStringable StrVar    = ()
    TValStringable _         = TypeError ('Text "Type is not a stringable TVal.")

type family StringyTVal (a :: *) (b :: *) :: Constraint where 
    StringyTVal a b = (ToTVal a b, TValStringable b)

-- Types that can be used as booleans
type family Truthy (a :: *) :: Constraint where 
    Truthy Bool      = ()
    Truthy TurtleVar = ()
    Truthy _         = TypeError ('Text "Not a Truthy type!")

type family TruthyTVal (a :: *) (b :: *) :: Constraint where 
    TruthyTVal a b = (ToTVal a b, Truthy b)

-- String Concatenation
(...) :: (StringyTVal a a', StringyTVal b b') => a -> b -> TVal StrVar
a ... b = TStrVar $ applyTOp ".." a b

infixr 5 ...

-- Equality operations

(.==) :: (ToTVal a a', ToTVal b b') => a -> b -> TVal Bool
a .== b = TBool $ applyTOp "==" a b

(~=) :: (ToTVal a a', ToTVal b b') => a -> b -> TVal Bool
a ~= b = TBool $ applyTOp "~=" a b

infix 4 .==
infix 4 ~=

-- Comparison operations

(.>) :: (NumericTVal a a', NumericTVal b b') => a -> b -> TVal Bool
a .> b = TBool $ applyTOp ">" a b

(.<) :: (NumericTVal a a', NumericTVal b b') => a -> b -> TVal Bool
a .< b = TBool $ applyTOp "<" a b

(.>=) :: (NumericTVal a a', NumericTVal b b') => a -> b -> TVal Bool
a .>= b = TBool $ applyTOp ">=" a b

(.<=) :: (NumericTVal a a', NumericTVal b b') => a -> b -> TVal Bool
a .<= b = TBool $ applyTOp "<=" a b
