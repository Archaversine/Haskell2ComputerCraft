module Turtle ( tPrint
              , writeProgram
              , module Turtle.Var
              , module Turtle.Types
              , module Turtle.Control
              , module Turtle.Movement
              ) where 

import Control.Monad.Writer (execWriter)

import Turtle.Var
import Turtle.Types
import Turtle.Control
import Turtle.Movement

writeProgram :: Turtle a -> FilePath -> IO ()
writeProgram prog name = writeFile name text 
    where text = execWriter prog 

tPrint :: TString a => a -> Turtle ()
tPrint text = lFunc "print" [tStr text]

