{-# LANGUAGE FlexibleInstances #-}

module Turtle.Movement ( ToolSide(..)
                       , refuel
                       , dig, digUp, digDown
                       , detect, detectUp, detectDown
                       , forward, back, up, down 
                       , turnLeft, turnRight
                       ) where 

import Turtle.Control
import Turtle.Types

class Refuel a where 
    refuel :: a -> Turtle ()

instance Refuel () where 
    refuel = const $ tFuncE "refuel"

instance Refuel (TVal Double) where 
    refuel amount = tFunc "refuel" [tStr amount]

data ToolSide = LeftSide | RightSide

instance Show ToolSide where 
    show LeftSide = "left"
    show RightSide = "right"

class Dig a where 
    dig     :: a -> Turtle (TVal Bool) 
    digUp   :: a -> Turtle (TVal Bool) 
    digDown :: a -> Turtle (TVal Bool)

instance Dig () where 
    dig     = const $ tFuncBoolE "dig"
    digUp   = const $ tFuncBoolE "digUp"
    digDown = const $ tFuncBoolE "digDown"

instance Dig ToolSide where 
    dig     side = tFuncBool "dig" [show $ show side]
    digUp   side = tFuncBool "digUp" [show $ show side]
    digDown side = tFuncBool "digDown" [show $ show side]

detect :: Turtle (TVal Bool)
detect = tFuncBoolE "detect"

detectUp :: Turtle (TVal Bool)
detectUp = tFuncBoolE "detectUp"

detectDown :: Turtle (TVal Bool)
detectDown = tFuncBoolE "detectDown"

forward :: Turtle (TVal Bool)
forward = tFuncBoolE "forward"

back :: Turtle (TVal Bool)
back = tFuncBoolE "back"

up :: Turtle (TVal Bool)
up = tFuncBoolE "up"

down :: Turtle (TVal Bool)
down = tFuncBoolE "down"

turnLeft :: Turtle (TVal Bool) 
turnLeft = tFuncBoolE "turnLeft"

turnRight :: Turtle (TVal Bool) 
turnRight = tFuncBoolE "turnRight"
