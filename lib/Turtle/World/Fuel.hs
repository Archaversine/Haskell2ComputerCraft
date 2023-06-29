{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Turtle.World.Fuel (refuel) where 

import Turtle.Types
import Turtle.Control

class Refuel a where 
    refuel :: a -> Turtle ()

instance {-# OVERLAPS #-} Refuel () where 
    refuel = const $ callTFuncE "refuel"

instance ToTVal a Double => Refuel a where 
    refuel amount = callTFunc "refuel" [tStr $ toTVal amount]
