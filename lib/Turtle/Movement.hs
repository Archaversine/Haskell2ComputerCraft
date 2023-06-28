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
    dig     :: a -> Turtle Bool 
    digUp   :: a -> Turtle Bool 
    digDown :: a -> Turtle Bool

instance Dig () where 
    dig     = const $ tFuncBoolE "dig"
    digUp   = const $ tFuncBoolE "digUp"
    digDown = const $ tFuncBoolE "digDown"

instance Dig ToolSide where 
    dig     side = tFuncBool "dig" [show $ show side]
    digUp   side = tFuncBool "digUp" [show $ show side]
    digDown side = tFuncBool "digDown" [show $ show side]

detect :: Turtle Bool 
detect = tFuncBool "detect" []

detectUp :: Turtle Bool 
detectUp = tFuncBool "detectUp" []

detectDown :: Turtle Bool 
detectDown = tFuncBool "detectDown" []

forward :: Turtle Bool 
forward = tFuncBool "forward" []

back :: Turtle Bool 
back = tFuncBool "back" []

up :: Turtle Bool 
up = tFuncBool "up" []

down :: Turtle Bool 
down = tFuncBool "down" []

turnLeft :: Turtle Bool 
turnLeft = tFuncBool "turnLeft" []

turnRight :: Turtle Bool 
turnRight = tFuncBool "turnRight" []
