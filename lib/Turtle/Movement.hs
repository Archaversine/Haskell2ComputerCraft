module Turtle.Movement ( refuel
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
    refuel = const $ tFunc "refuel" []

instance Refuel Int where 
    refuel amount = tFunc "refuel" [tStr amount]

instance Refuel Float where
    refuel amount = tFunc "refuel" [tStr amount]

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
detect = tFuncBool "detect" []

detectUp :: Turtle (TVal Bool)
detectUp = tFuncBool "detectUp" []

detectDown :: Turtle (TVal Bool)
detectDown = tFuncBool "detectDown" []

forward :: Turtle (TVal Bool)
forward = tFuncBool "forward" []

back :: Turtle (TVal Bool)
back = tFuncBool "back" []

up :: Turtle (TVal Bool)
up = tFuncBool "up" []

down :: Turtle (TVal Bool)
down = tFuncBool "down" []

turnLeft :: Turtle (TVal Bool) 
turnLeft = tFuncBool "turnLeft" []

turnRight :: Turtle (TVal Bool) 
turnRight = tFuncBool "turnRight" []
