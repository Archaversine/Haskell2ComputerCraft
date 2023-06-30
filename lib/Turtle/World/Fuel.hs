{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Turtle.World.Fuel (refuel, getFuelLevel, getFuelLimit) where 

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

instance NumericTVal num a' => Refuel num where 
    refuel (showTVal -> amount) = callTFunc "refuel" [amount]

-- | Returns the current fuel level of the turtle, this is the number of blocks the turtle can move.
getFuelLevel :: TVal Number
getFuelLevel = tFuncNumberE "getFuelLevel"

-- | Returns the maximum amount of fuel a turtle can store. 
-- By default, 20,000 for regular turtles, 100,000 for advanced
getFuelLimit :: TVal Number
getFuelLimit = tFuncNumberE "getFuelLimit"
