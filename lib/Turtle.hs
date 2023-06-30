module Turtle ( tPrint
              , writeProgram
              , printProgram
              , module Turtle.Var
              , module Turtle.Types
              , module Turtle.World
              , module Turtle.Control
              , module Turtle.Term
              ) where 

import Control.Monad.Writer (execWriter)

import Turtle.Var
import Turtle.Types
import Turtle.World
import Turtle.Control
import Turtle.Term

writeProgram :: Turtle a -> FilePath -> IO ()
writeProgram prog name = writeFile name text 
    where text = execWriter prog 

printProgram :: Turtle a -> IO ()
printProgram = putStr . execWriter

tPrint :: ToTVal a a' => a -> Turtle ()
tPrint text = callFunc "print" [showTVal text]

