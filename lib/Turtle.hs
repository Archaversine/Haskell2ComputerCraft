module Turtle ( tPrint
              , writeProgram
              , module Turtle.Var
              , module Turtle.Types
              , module Turtle.World
              , module Turtle.Control
              ) where 

import Control.Monad.Writer (execWriter)

import Turtle.Var
import Turtle.Types
import Turtle.World
import Turtle.Control

writeProgram :: Turtle a -> FilePath -> IO ()
writeProgram prog name = writeFile name text 
    where text = execWriter prog 

tPrint :: ToTVal a a' => a -> Turtle ()
tPrint text = callFunc "print" [showTVal text]

