{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Turtle.World.Fuel (refuel) where 

import Turtle.Types
import Turtle.Control

class Refuel a where 

    -- | If the current selected slot contains a fuel item, it will consume it to give the turtle the ability to move. 
    -- If the current slot doesn't contain a fuel item, it returns False. 
    -- If a quantity is specified, it will refuel only up to that many times, 
    -- otherwise it will consume the entire slot.
    -- Fuel level information for items: https://computercraft.info/wiki/Turtle.refuel#Fuel_Values
    refuel :: a -> Turtle ()

instance {-# OVERLAPS #-} Refuel () where 
    refuel = const $ callTFuncE "refuel"

instance ToTVal a Double => Refuel a where 
    refuel (showTVal -> amount) = callTFunc "refuel" [amount]

