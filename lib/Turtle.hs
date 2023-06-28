module Turtle ( tPrint
              , refuel
              , dig, digUp, digDown
              , detect, detectUp, detectDown
              , forward, back, up, down
              , turnLeft, turnRight
              , writeProgram
              , module Turtle.Var
              , module Turtle.Types
              , module Turtle.Control
              ) where 

import Control.Monad.Writer (execWriter)

import Turtle.Var
import Turtle.Types
import Turtle.Control

writeProgram :: Turtle a -> FilePath -> IO ()
writeProgram prog name = writeFile name text 
    where text = execWriter prog 

tPrint :: TString a => a -> Turtle ()
tPrint text = lFunc "print" [tStr text]

refuel :: (Show a, Num a) => Maybe a -> Turtle ()
refuel Nothing       = tFunc "refuel" []
refuel (Just amount) = tFunc "refuel" [show amount]

dig :: Maybe ToolSide -> Turtle Bool
dig Nothing     = tFuncBool "dig" []
dig (Just side) = tFuncBool "dig" [show $ show side]

digUp :: Maybe ToolSide -> Turtle Bool
digUp Nothing     = tFuncBool "digUp" []
digUp (Just side) = tFuncBool "digUp" [show $ show side]

digDown :: Maybe ToolSide -> Turtle Bool
digDown Nothing     = tFuncBool "digDown" []
digDown (Just side) = tFuncBool "digDown" [show $ show side]

detect :: Turtle Bool 
detect = tFuncBool "detect" []

detectUp :: Turtle Bool 
detectUp = tFuncBool "detectUp" []

detectDown :: Turtle Bool 
detectDown = tFuncBool "detectDown" []

forward :: Turtle Bool 
forward = tFuncBool "forward" []

back :: Turtle Bool 
back = tFuncBool "back" []

up :: Turtle Bool 
up = tFuncBool "up" []

down :: Turtle Bool 
down = tFuncBool "down" []

turnLeft :: Turtle Bool 
turnLeft = tFuncBool "turnLeft" []

turnRight :: Turtle Bool 
turnRight = tFuncBool "turnRight" []