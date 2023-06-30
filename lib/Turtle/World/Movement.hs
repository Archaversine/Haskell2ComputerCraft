module Turtle.World.Movement ( moveForward, moveBack, moveUp, moveDown 
                             , moveForwardM, moveBackM, moveUpM, moveDownM
                             , turnLeft, turnRight
                             , turnLeftM, turnRightM
                             ) where 

import Turtle.Control
import Turtle.Types

-- | Try to move the turtle forward
moveForward :: TVal Bool
moveForward = tFuncBoolE "forward"

-- | Try to move the turtle back
moveBack :: TVal Bool
moveBack = tFuncBoolE "back"

-- | Try to move the turtle up
moveUp :: TVal Bool
moveUp = tFuncBoolE "up"

-- | Try to move the turtle down
moveDown :: TVal Bool
moveDown = tFuncBoolE "down"

-- | Turn the turtle left
turnLeft :: TVal Bool
turnLeft = tFuncBoolE "turnLeft"

-- | Turn the turtle right
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
