module Turtle.World.Movement ( moveForward, moveBack, moveUp, moveDown 
                             , moveForwardM, moveBackM, moveUpM, moveDownM
                             , turnLeft, turnRight
                             , turnLeftM, turnRightM
                             ) where 

import Turtle.Control
import Turtle.Types

moveForward :: TVal Bool
moveForward = tFuncBoolE "forward"

moveBack :: TVal Bool
moveBack = tFuncBoolE "back"

moveUp :: TVal Bool
moveUp = tFuncBoolE "up"

moveDown :: TVal Bool
moveDown = tFuncBoolE "down"

turnLeft :: TVal Bool
turnLeft = tFuncBoolE "turnLeft"

turnRight :: TVal Bool
turnRight = tFuncBoolE "turnRight"

-- Turtle Actions

moveForwardM :: Turtle ()
moveForwardM = callTFuncE "forward"

moveBackM :: Turtle ()
moveBackM = callTFuncE "back"

moveUpM :: Turtle ()
moveUpM = callTFuncE "up"

moveDownM :: Turtle () 
moveDownM = callTFuncE "down"

turnLeftM :: Turtle ()
turnLeftM = turtle turnLeft

turnRightM :: Turtle ()
turnRightM = turtle turnRight
