module Turtle.World.Blocks (ToolSide(..), Dig(..)) where 

import Turtle.Types
import Turtle.Control

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
