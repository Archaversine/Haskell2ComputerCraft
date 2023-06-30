module Turtle.World.Blocks (ToolSide(..), Dig(..)) where 

import Turtle.Types
import Turtle.Control

data ToolSide = LeftSide | RightSide

instance Show ToolSide where 
    show LeftSide = "left"
    show RightSide = "right"

class Dig a where 
    
    -- | Breaks the block in front. With hoe: tills dirt in front of it.
    dig      :: a -> TVal Bool 

    -- | Breaks the block above.
    digUp    :: a -> TVal Bool 

    -- | Breaks the block below. With hoe: tills the dirt beneat the space below it.
    digDown  :: a -> TVal Bool

    digM :: a -> Turtle ()
    digM = turtle . dig

    digUpM :: a -> Turtle ()
    digUpM = turtle . digUp

    digDownM :: a -> Turtle ()
    digDownM = turtle . digDown

instance Dig () where 
    dig      = const $ tFuncBoolE "dig"
    digUp    = const $ tFuncBoolE "digUp"
    digDown  = const $ tFuncBoolE "digDown"

instance Dig ToolSide where 
    dig     side  = tFuncBool "dig" [show $ show side]
    digUp   side  = tFuncBool "digUp" [show $ show side]
    digDown side  = tFuncBool "digDown" [show $ show side]

