{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Turtle.World.Blocks ( ToolSide(..)
                           , Dig(..) 
                           , BlockData(..)
                           , place, placeM 
                           , placeSign, placeSignM
                           , placeUp, placeUpM
                           , placeDown, placeDownM
                           , detect 
                           , detectUp 
                           , detectDown
                           , inspect
                           , inspectUp
                           , inspectDown
                           , compareFront 
                           , compareUp 
                           , compareDown
                           , dropFront, dropFrontM
                           , dropUp, dropUpM 
                           , dropDown, dropDownM
                           , suck, suckM 
                           , suckUp, suckUpM
                           , suckDown, suckDownM
                           ) where 

import Control.Monad.Writer (tell)

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

-- | Places a block of the selected slot in front of the turtle.
-- Collects water or lava if the currently selected slot is an empty bucket.
place :: TVal Bool
place = tFuncBoolE "place"

placeM :: Turtle ()
placeM = turtle place

-- | Places a block of the selected slot in front of the turtle.
-- Collects water or lava if the currently selected slot is an empty bucket.
-- Applies sign text to sign if sign placed.
placeSign :: StringyTVal string a' => string -> TVal Bool 
placeSign (showTVal -> text) = tFuncBool "place" [text]

placeSignM :: StringyTVal string a' => string -> Turtle ()
placeSignM = turtle . placeSign

-- | Places a block of the selected slot above. 
-- Collects water or lava if the currently selected slot is an empty bucket.
placeUp :: TVal Bool 
placeUp = tFuncBoolE "placeUp"

placeUpM :: Turtle ()
placeUpM = turtle placeUp

-- | Places a block of the selected slot below. 
-- Collects water or lava if the currently selected slot is an empty bucket.
placeDown :: TVal Bool 
placeDown = tFuncBoolE "placeDown"

placeDownM :: Turtle ()
placeDownM = turtle placeDown

-- | Detects if there is a block in front. Does NOT detect mobs.
detect :: TVal Bool 
detect = tFuncBoolE "detect"

-- | Detects if there is a block above 
detectUp :: TVal Bool 
detectUp = tFuncBoolE "detectUp"

-- | Detects if there is a block below 
detectDown :: TVal Bool
detectDown = tFuncBoolE "detectDown"

-- TODO: Add inspect functions

data BlockData = BlockData { blockName :: TVal StrVar, blockMetadata :: TVal Number }

createBlockData :: String -> BlockData
createBlockData varname = BlockData name metadata
    where name     = TStrVar $ varname <> ".name"
          metadata = TNumber $ varname <> ".metadata"

inspect :: (String, String) -> Turtle (TVal Bool, BlockData)
inspect (retName, dataName) = do 
    let name = tfName "inspect" []

    tell $ "local " <> retName <> ", " <> dataName <> " = " <> name <> "\n"
    return (TBool retName, createBlockData dataName)

inspectUp :: (String, String) -> Turtle (TVal Bool, BlockData) 
inspectUp (retName, dataName) = do 
    let name = tfName "inspectUp" []

    tell $ "local " <> retName <> ", " <> dataName <> " = " <> name <> "\n"
    return (TBool retName, createBlockData dataName)

inspectDown :: (String, String) -> Turtle (TVal Bool, BlockData)
inspectDown (retName, dataName) = do 
    let name = tfName "inspectDown" []

    tell $ "local " <> retName <> ", " <> dataName <> " = " <> name <> "\n"
    return (TBool retName, createBlockData dataName)

-- | Detects if the block in front is the same as the one in the currently selected slot.
compareFront :: TVal Bool 
compareFront = tFuncBoolE "compare"

-- | Detects if the block above is the same as the one in the currently selected slot.
compareUp :: TVal Bool 
compareUp = tFuncBoolE "compareUp"

-- | Detects if the block below is the same as the one in the currently selected slot.
compareDown :: TVal Bool 
compareDown = tFuncBoolE "compareDown"

class Drop a where 
    
    -- | Drops all the items in the selected slot, or specified, drops n items
    dropFront :: a -> TVal Bool

    -- | Drops all the items in the selected slot, or specified, drops n items
    dropUp :: a -> TVal Bool 

    -- | Drops all the items in the selected slot, or specified, drops n items
    dropDown :: a -> TVal Bool

    dropFrontM :: a -> Turtle ()
    dropFrontM = turtle . dropFront

    dropUpM :: a -> Turtle ()
    dropUpM = turtle . dropUp

    dropDownM :: a -> Turtle ()
    dropDownM = turtle . dropDown

instance {-# OVERLAPS #-} Drop () where 
    dropFront = const $ tFuncBoolE "drop"
    dropUp    = const $ tFuncBoolE "dropUp"
    dropDown  = const $ tFuncBoolE "dropDown"

instance NumericTVal num a' => Drop num where 
    dropFront (showTVal -> amount) = tFuncBool "drop" [amount]
    dropUp    (showTVal -> amount) = tFuncBool "dropUp" [amount]
    dropDown  (showTVal -> amount) = tFuncBool "dropDown" [amount]

class Suck a where 
    
    -- | Picks up an item stack of any number, from the ground or an inventory in front of the turtle, 
    -- then places it in the selected slot.
    -- If the turtle can't pick up the item, the function returns false.
    suck :: a -> TVal Bool 

    -- | Picks up an item stack of any number, from the ground or an inventory in front of the turtle, 
    -- then places it in the selected slot.
    -- If the turtle can't pick up the item, the function returns false.
    suckUp :: a -> TVal Bool

    -- | Picks up an item stack of any number, from the ground or an inventory in front of the turtle, 
    -- then places it in the selected slot.
    -- If the turtle can't pick up the item, the function returns false.
    suckDown :: a -> TVal Bool

    suckM :: a -> Turtle ()
    suckM = turtle . suck

    suckUpM :: a -> Turtle ()
    suckUpM = turtle . suckUp

    suckDownM :: a -> Turtle ()
    suckDownM = turtle . suckDown

instance {-# OVERLAPS #-} Suck () where 
    suck     = const $ tFuncBoolE "suck"
    suckUp   = const $ tFuncBoolE "suckUp"
    suckDown = const $ tFuncBoolE "suckDown"

instance NumericTVal num a' => Suck num where 
    suck (showTVal -> amount) = tFuncBool "suck" [amount]
    suckUp (showTVal -> amount) = tFuncBool "suckUp" [amount]
    suckDown (showTVal -> amount) = tFuncBool "suckDown" [amount]
