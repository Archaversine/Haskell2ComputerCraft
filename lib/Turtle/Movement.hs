{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Turtle.Movement ( ToolSide(..)
                       , Refuel(..)
                       , Dig(..)
                       , detect, detectUp, detectDown
                       , detectM, detectUpM, detectDownM
                       , moveForward, moveBack, moveUp, moveDown 
                       , moveForwardM, moveBackM, moveUpM, moveDownM
                       , turnLeft, turnRight
                       , turnLeftM, turnRightM
                       ) where 

import Turtle.Control
import Turtle.Types

class Refuel a where 
    refuel :: a -> Turtle ()

instance {-# OVERLAPS #-} Refuel () where 
    refuel = const $ callTFuncE "refuel"

instance ToTVal a Double => Refuel a where 
    refuel amount = callTFunc "refuel" [tStr $ toTVal amount]

data ToolSide = LeftSide | RightSide

instance Show ToolSide where 
    show LeftSide = "left"
    show RightSide = "right"

class Dig a where 
    dig      :: a -> TVal Bool 
    digUp    :: a -> TVal Bool 
    digDown  :: a -> TVal Bool

    digM     :: a -> Turtle ()
    digUpM   :: a -> Turtle ()
    digDownM :: a -> Turtle ()

instance Dig () where 
    dig      = const $ tFuncBoolE "dig"
    digUp    = const $ tFuncBoolE "digUp"
    digDown  = const $ tFuncBoolE "digDown"

    digM     = const $ callTFuncE "dig"
    digUpM   = const $ callTFuncE "digUp"
    digDownM = const $ callTFuncE "digDown"

instance Dig ToolSide where 
    dig     side  = tFuncBool "dig" [show $ show side]
    digUp   side  = tFuncBool "digUp" [show $ show side]
    digDown side  = tFuncBool "digDown" [show $ show side]

    digM side     = callTFunc "dig" [show $ show side]
    digUpM side   = callTFunc "digUp" [show $ show side]
    digDownM side = callTFunc "digDown" [show $ show side]

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

moveForward :: TVal Bool
moveForward = tFuncBoolE "forward"

moveBack :: TVal Bool
moveBack = tFuncBoolE "back"

moveUp :: TVal Bool
moveUp = tFuncBoolE "up"

moveDown :: TVal Bool
moveDown = tFuncBoolE "down"

moveForwardM :: Turtle ()
moveForwardM = callTFuncE "forward"

moveBackM :: Turtle ()
moveBackM = callTFuncE "back"

moveUpM :: Turtle ()
moveUpM = callTFuncE "up"

moveDownM :: Turtle () 
moveDownM = callTFuncE "down"

turnLeft :: TVal Bool
turnLeft = tFuncBoolE "turnLeft"

turnRight :: TVal Bool
turnRight = tFuncBoolE "turnRight"

turnLeftM :: Turtle ()
turnLeftM = turtle turnLeft

turnRightM :: Turtle ()
turnRightM = turtle turnRight
