{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Turtle.Types ( Turtle
                    , ToolSide(..)
                    , TString(..)
                    , TurtleVar(..)
                    , TVal(..)
                    , ToTVal(..)
                    , TNum(..)
                    , leftSide
                    , rightSide
                    ) where

import Control.Monad.Writer

import Data.Kind (Constraint)
import GHC.TypeLits

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

newtype TurtleVar = TurtleVar String

instance TString TurtleVar where 
    tStr (TurtleVar s) = s

class ToTVal a b | a -> b where 
    toTVal :: a -> TVal b

instance ToTVal Float Float where 
    toTVal = TFloat . show

instance ToTVal Bool Bool where 
    toTVal True = TBool "true"
    toTVal False = TBool "false"

instance ToTVal String String where 
    toTVal = TStr

instance ToTVal TurtleVar TurtleVar where 
    toTVal (TurtleVar name) = TTVar name

instance ToTVal (TVal a) a where 
    toTVal = id

data TVal a where 
    TFloat :: String -> TVal Float
    TBool  :: String -> TVal Bool
    TStr   :: String -> TVal String
    TTVar  :: String -> TVal TurtleVar

instance TString (TVal a) where 
    tStr (TFloat x) = x
    tStr (TBool x)  = x
    tStr (TStr x)   = show x
    tStr (TTVar x)  = x

type family NotString a :: Constraint where 
    NotString String = TypeError ('Text "Type cannot be a String")
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

instance (NotString a, NotString b) => TNum a b Float where 
    tAdd a b = TFloat $ "(" <> tStr (toTVal a) <> " + " <> tStr (toTVal b) <> ")"
    tSub a b = TFloat $ "(" <> tStr (toTVal a) <> " - " <> tStr (toTVal b) <> ")"
    tMul a b = TFloat $ "(" <> tStr (toTVal a) <> " * " <> tStr (toTVal b) <> ")"
    tDiv a b = TFloat $ "(" <> tStr (toTVal a) <> " / " <> tStr (toTVal b) <> ")"
