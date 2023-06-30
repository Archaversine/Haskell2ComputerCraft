{-# LANGUAGE ViewPatterns #-}

module Turtle.World.Attack ( attack, attackUp, attackDown 
                           , attackM, attackUpM, attackDownM 
                           ) where 

import Turtle.World.Blocks (ToolSide(..))

import Turtle.Control
import Turtle.Types

class Attack a where 

    -- | Attacks in front of the turtle.
    attack     :: a -> TVal Bool
    
    -- | Attacks above the turtle.
    attackUp   :: a -> TVal Bool

    -- | Attacks under the turtle.
    attackDown :: a -> TVal Bool

    attackM     :: a -> Turtle ()
    attackM = turtle . attack

    attackUpM   :: a -> Turtle ()
    attackUpM = turtle . attackUp

    attackDownM :: a -> Turtle ()
    attackDownM = turtle . attackDown

instance Attack () where 
    attack     = const $ tFuncBoolE "attack"
    attackUp   = const $ tFuncBoolE "attackUp"
    attackDown = const $ tFuncBoolE "attackDown"

instance Attack ToolSide where 
    attack (show . show -> side)     = tFuncBool "attack" [side]
    attackUp (show . show -> side)   = tFuncBool "attackUp" [side]
    attackDown (show . show -> side) = tFuncBool "attackDown" [side]
