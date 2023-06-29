{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Turtle.Types ( tStr 
                    , Turtle
                    , TurtleVar(..)
                    , TVal(..)
                    , ToTVal(..)
                    , TNum(..)
                    , NotString
                    , TruthyTVal
                    , (...)
                    , (.==), (~=)
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

tStr :: TVal a -> String 
tStr (TDouble x) = x
tStr (TBool x)   = x
tStr (TStr x)    = show x
tStr (TTVar x)   = x
tStr (TStrVar x) = x

type family NotString (a :: *) :: Constraint where 
    NotString String = TypeError ('Text "Type cannot be a String or StrVar!")
    NotString StrVar = TypeError ('Text "Type cannot be a String or StrVar!")
    NotString _      = ()

class (NotString a, NotString b) => TNum a b c | a b -> c where 
    tAdd :: (ToTVal a a', ToTVal b b') => a -> b -> TVal c
    tSub :: (ToTVal a a', ToTVal b b') => a -> b -> TVal c
    tMul :: (ToTVal a a', ToTVal b b') => a -> b -> TVal c
    tDiv :: (ToTVal a a', ToTVal b b') => a -> b -> TVal c

    (.+) :: (ToTVal a a', ToTVal b b') => a -> b -> TVal c
    (.+) = tAdd

    (.-) :: (ToTVal a a', ToTVal b b') => a -> b -> TVal c
    (.-) = tSub

    (.*) :: (ToTVal a a', ToTVal b b') => a -> b -> TVal c
    (.*) = tMul

    (./) :: (ToTVal a a', ToTVal b b') => a -> b -> TVal c
    (./) = tDiv

infixl 6 .+
infixl 6 .-
infixl 7 .*
infixl 7 ./

instance (NotString a, NotString b) => TNum a b Double where 
    tAdd a b = TDouble $ "(" <> tStr (toTVal a) <> " + " <> tStr (toTVal b) <> ")"
    tSub a b = TDouble $ "(" <> tStr (toTVal a) <> " - " <> tStr (toTVal b) <> ")"
    tMul a b = TDouble $ "(" <> tStr (toTVal a) <> " * " <> tStr (toTVal b) <> ")"
    tDiv a b = TDouble $ "(" <> tStr (toTVal a) <> " / " <> tStr (toTVal b) <> ")"

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
a ... b = TStrVar $ tStr (toTVal a) <> " .. " <> tStr (toTVal b)

infixr 5 ...

-- Equality operations

(.==) :: (ToTVal a a', ToTVal b b') => a -> b -> TVal Bool
a .== b = TBool $ "(" <> tStr (toTVal a) <> " == " <> tStr (toTVal b) <> ")"

(~=) :: (ToTVal a a', ToTVal b b') => a -> b -> TVal Bool
a ~= b = TBool $ "(" <> tStr (toTVal a) <> " ~= " <> tStr (toTVal b) <> ")"

infix 4 .==
infix 4 ~=
