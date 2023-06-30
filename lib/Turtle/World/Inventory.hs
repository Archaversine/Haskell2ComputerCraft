{-# LANGUAGE ViewPatterns #-}

module Turtle.World.Inventory ( select, selectM 
                              , getSelectedSlot 
                              , getCurrentItemCount 
                              , getItemCount
                              , getCurrentItemSpace
                              , getItemSpace
                              , equipLeft 
                              , equipRight
                              ) where

import Turtle.Types 
import Turtle.Control 

-- | Make the turtle select slot slotNum.
-- 1 is top left, 16 is bottom right.
select :: NumericTVal num a' => num -> TVal Bool 
select (showTVal -> slot) = tFuncBool "select" [slot]

-- | Indicates the currently selected inventory slot.
getSelectedSlot :: TVal Double 
getSelectedSlot = tFuncDoubleE "getSelectedSlot"

-- | Counts how many items are in the currently selected item slot.
getCurrentItemCount :: TVal Double 
getCurrentItemCount = tFuncDoubleE "getItemCount"

-- | Counts how many items are in the specified item slot.
getItemCount :: NumericTVal num a' => num -> TVal Double 
getItemCount (showTVal -> slot) = tFuncDouble "getItemCount" [slot]

-- | Counts how many remaining items you need to fill the stack in the currently selected item slot.
getCurrentItemSpace :: TVal Double 
getCurrentItemSpace = tFuncDoubleE "getItemSpace"
 
-- | Counts how many remaining items you need to fill the stack in the specified item slot.
getItemSpace :: NumericTVal num a' => num -> TVal Double 
getItemSpace (showTVal -> slot) = tFuncDouble "getItemSpace" [slot]

-- TODO: Add getItemDetail

-- | Attempts to equip an item in the current slot to the turtle's left side, 
-- switching the previously equipped item back into the inventory.
equipLeft :: TVal Bool 
equipLeft = tFuncBoolE "equipLeft"

-- | Attempts to equip an item in the current slot to the turtle's left side, 
-- switching the previously equipped item back into the inventory.
equipRight :: TVal Bool 
equipRight = tFuncBoolE "equipRight"

-- | Counts how many items are in the currently selected slot

-- Turtle Actions

selectM :: NumericTVal num a' => num -> Turtle ()
selectM = turtle . select
