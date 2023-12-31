{-# LANGUAGE ViewPatterns #-}

module Turtle.World.Inventory ( select, selectM 
                              , getSelectedSlot 
                              , getCurrentItemCount 
                              , getItemCount
                              , getCurrentItemSpace
                              , getItemSpace
                              , equipLeft, equipLeftM
                              , equipRight, equipRightM
                              , transferTo, transferToM
                              , transferAllTo, transferAllToM
                              ) where

import Turtle.Types 
import Turtle.Control 

-- | Make the turtle select slot slotNum.
-- 1 is top left, 16 is bottom right.
select :: NumericTVal num a' => num -> TVal Bool 
select (showTVal -> slot) = tFuncBool "select" [slot]

selectM :: NumericTVal num a' => num -> Turtle ()
selectM = turtle . select

-- | Indicates the currently selected inventory slot.
getSelectedSlot :: TVal Number
getSelectedSlot = tFuncNumberE "getSelectedSlot"

-- | Counts how many items are in the currently selected item slot.
getCurrentItemCount :: TVal Number 
getCurrentItemCount = tFuncNumberE "getItemCount"

-- | Counts how many items are in the specified item slot.
getItemCount :: NumericTVal num a' => num -> TVal Number
getItemCount (showTVal -> slot) = tFuncNumber "getItemCount" [slot]

-- | Counts how many remaining items you need to fill the stack in the currently selected item slot.
getCurrentItemSpace :: TVal Number
getCurrentItemSpace = tFuncNumberE "getItemSpace"
 
-- | Counts how many remaining items you need to fill the stack in the specified item slot.
getItemSpace :: NumericTVal num a' => num -> TVal Number
getItemSpace (showTVal -> slot) = tFuncNumber "getItemSpace" [slot]

-- TODO: Add getItemDetail

-- | Attempts to equip an item in the current slot to the turtle's left side, 
-- switching the previously equipped item back into the inventory.
equipLeft :: TVal Bool 
equipLeft = tFuncBoolE "equipLeft"

equipLeftM :: Turtle ()
equipLeftM = turtle equipLeft

-- | Attempts to equip an item in the current slot to the turtle's left side, 
-- switching the previously equipped item back into the inventory.
equipRight :: TVal Bool 
equipRight = tFuncBoolE "equipRight"

equipRightM :: Turtle ()
equipRightM = turtle equipRight

-- | Transfers n items from the selected slot to slot
transferTo :: (NumericTVal num1 a', NumericTVal num2 b')
           => num1 -> num2 -> TVal Bool 
transferTo (showTVal -> amount) (showTVal -> slot) = do 
    tFuncBool "transferTo" [slot, amount]

transferToM :: (NumericTVal num1 a', NumericTVal num2 b') 
            => num1 -> num2 -> Turtle () 
transferToM amount slot = turtle $ transferTo amount slot

-- Transfers all items from the selected slot to slot
transferAllTo :: NumericTVal num a' => num -> TVal Bool 
transferAllTo (showTVal -> slot) = tFuncBool "transferTo" [slot]

transferAllToM :: NumericTVal num a' => num -> Turtle ()
transferAllToM = turtle . transferAllTo
