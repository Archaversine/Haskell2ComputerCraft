module Turtle.Var ( TurtleVar(..)
                  , declareLocal
                  , defineLocal
                  , setVar
                  ) where

import Control.Monad.Writer

import Turtle.Types

newtype TurtleVar = TurtleVar String

instance TString TurtleVar where 
    tStr (TurtleVar s) = s

declareLocal :: String -> Turtle TurtleVar
declareLocal name = do 
    tell $ "local " <> name <> "\n"
    return $ TurtleVar name

defineLocal :: TString a => String -> a -> Turtle TurtleVar
defineLocal name value = do 
    tell $ "local " <> name <> " = " <> tStr value <> "\n"
    return $ TurtleVar name

setVar :: TString a => TurtleVar -> a -> Turtle ()
setVar (TurtleVar name) value = tell $ name <> " = " <> tStr value <> "\n"