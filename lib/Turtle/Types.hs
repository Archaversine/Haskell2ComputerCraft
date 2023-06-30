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
                    , Number
                    , ToTVal(..)
                    , NotString
                    , TruthyTVal
                    , NumericTVal
                    , StringyTVal
                    , showTVal
                    , fromFractional
                    , tAdd, tSub, tMul, tDiv
                    , (.+), (.-), (.*), (./), (%)
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

type Number = Integer

instance ToTVal Number Number where 
    toTVal = TNumber . show

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
    TNumber :: String -> TVal Number
    TBool   :: String -> TVal Bool
    TStr    :: String -> TVal String
    TTVar   :: String -> TVal TurtleVar
    TStrVar :: String -> TVal StrVar

instance Show (TVal a) where 
    show (TNumber x) = x
    show (TBool x)   = x
    show (TStr x)    = show x
    show (TTVar x)   = x
    show (TStrVar x) = x

showTVal :: ToTVal any a' => any -> String
showTVal = show . toTVal

fromFractional :: (Show a, Fractional a) => a -> TVal Number
fromFractional x = TNumber $ show x

type family NotString (a :: *) :: Constraint where 
    NotString String = TypeError ('Text "Type cannot be a String or StrVar!")
    NotString StrVar = TypeError ('Text "Type cannot be a String or StrVar!")
    NotString _      = ()

type family NotBool (a :: *) :: Constraint where 
    NotBool Bool = TypeError ('Text "Type cannot be a Bool!")
    NotBool _    = ()

type family TNum (a :: *) :: Constraint where 
    TNum Number    = () 
    TNum TurtleVar = () 
    TNum _         = TypeError ('Text "Type must be a TNum type")

type family NumericTVal (a :: *) (b :: *) :: Constraint where 
    NumericTVal a b = (ToTVal a b, TNum b)

applyTOp :: (ToTVal any1 a', ToTVal any2 b') => String -> any1 -> any2 -> String
applyTOp op a b = "(" <> showTVal a <> " " <> op <> " " <> showTVal b <> ")"

tAdd :: (NumericTVal num1 a', NumericTVal num2 b') => num1 -> num2 -> TVal Number
tAdd a b = TNumber $ applyTOp "+" a b

tSub :: (NumericTVal num1 a', NumericTVal num2 b') => num1 -> num2 -> TVal Number
tSub a b = TNumber $ applyTOp "-" a b

tMul :: (NumericTVal num1 a', NumericTVal num2 b') => num1 -> num2 -> TVal Number
tMul a b = TNumber $ applyTOp "*" a b

tDiv :: (NumericTVal num1 a', NumericTVal num2 b') => num1 -> num2 -> TVal Number
tDiv a b = TNumber $ applyTOp "/" a b

(.+) :: (NumericTVal num1 a', NumericTVal num2 b') => num1 -> num2 -> TVal Number
(.+) = tAdd

(.-) :: (NumericTVal num1 a', NumericTVal num2 b') => num1 -> num2 -> TVal Number
(.-) = tSub

(.*) :: (NumericTVal num1 a', NumericTVal num2 b') => num1 -> num2 -> TVal Number
(.*) = tMul

(./) :: (NumericTVal num1 a', NumericTVal num2 b') => num1 -> num2 -> TVal Number
(./) = tDiv

(%) :: (NumericTVal num1 a', NumericTVal num2 b') => num1 -> num2 -> TVal Number
a % b = TNumber $ applyTOp "%" a b

infixl 6 .+
infixl 6 .-
infixl 7 .*
infixl 7 ./

-- Types that can be used as booleans
type family Truthy (a :: *) :: Constraint where 
    Truthy Bool      = ()
    Truthy TurtleVar = ()
    Truthy _         = TypeError ('Text "Not a Truthy type!")

type family TruthyTVal (a :: *) (b :: *) :: Constraint where 
    TruthyTVal a b = (ToTVal a b, Truthy b)

type family Stringy (a :: *) :: Constraint where 
    Stringy String    = () 
    Stringy TurtleVar = ()
    Stringy _         = TypeError ('Text "Not a Stringy Type!")

type family StringyTVal (a :: *) (b :: *) :: Constraint where 
    StringyTVal a b = (ToTVal a b, Stringy b)

type family NonBoolTVal (a :: *) (b :: *) :: Constraint where 
    NonBoolTVal a b = (ToTVal a b, NotBool b)

-- String Concatenation
(...) :: (NonBoolTVal any1 a', NonBoolTVal any2 b') => any1 -> any2 -> TVal StrVar
a ... b = TStrVar $ applyTOp ".." a b

infixr 5 ...

-- Equality operations

(.==) :: (ToTVal bool1 a', ToTVal bool2 b') => bool1 -> bool2 -> TVal Bool
a .== b = TBool $ applyTOp "==" a b

(~=) :: (ToTVal bool1 a', ToTVal bool2 b') => bool1 -> bool2 -> TVal Bool
a ~= b = TBool $ applyTOp "~=" a b

infix 4 .==
infix 4 ~=

-- Comparison operations

(.>) :: (NumericTVal bool1 a', NumericTVal bool2 b') => bool1 -> bool2 -> TVal Bool
a .> b = TBool $ applyTOp ">" a b

(.<) :: (NumericTVal bool1 a', NumericTVal bool2 b') => bool1 -> bool2 -> TVal Bool
a .< b = TBool $ applyTOp "<" a b

(.>=) :: (NumericTVal bool1 a', NumericTVal bool2 b') => bool1 -> bool2 -> TVal Bool
a .>= b = TBool $ applyTOp ">=" a b

(.<=) :: (NumericTVal bool1 a', NumericTVal bool2 b') => bool1 -> bool2 -> TVal Bool
a .<= b = TBool $ applyTOp "<=" a b
