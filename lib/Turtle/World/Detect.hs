module Turtle.World.Detect ( detect 
                           , detectUp 
                           , detectDown
                           , detectM 
                           , detectUpM
                           , detectDownM
                           ) where 

import Turtle.Types
import Turtle.Control

detect :: TVal Bool
detect = tFuncBoolE "detect"

detectUp :: TVal Bool
detectUp = tFuncBoolE "detectUp"

detectDown :: TVal Bool
detectDown = tFuncBoolE "detectDown"

detectM :: Turtle ()
detectM = turtle detect

detectUpM :: Turtle ()
detectUpM = turtle detectUp

detectDownM :: Turtle ()
detectDownM = turtle detectDown
