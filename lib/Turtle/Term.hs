{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}

module Turtle.Term ( MonitorPos(..) 
                   , wrapTerm 
                   , wrapPeripheral
                   , write 
                   , blit
                   , clear
                   , clearLine
                   , getCursorPos
                   , setCursorPos
                   , setCursorBlink
                   , isColor
                   , getSize
                   , scroll
                   , setTextColor
                   , getTextColor
                   , setBackgroundColor
                   , getBackgroundColor
                   , setTextScale
                   , setVisible
                   , redraw
                   , restoreCursor
                   , getPosition
                   , reposition
                   , repositionWH
                   ) where

import Control.Monad.Writer (tell)

import Turtle.Types
import Turtle.Control

data MonitorPos = MonUp 
                | MonDown 
                | MonLeft 
                | MonRight 
                | MonFront 
                | MonBack

instance Show MonitorPos where 
    show MonUp    = "up"
    show MonDown  = "down"
    show MonLeft  = "left"
    show MonRight = "right"
    show MonFront = "front"
    show MonBack  = "back"

data TerminalType = Terminal | Monitor | Window

data TermType (t :: TerminalType) where 
    TTTerminal   :: TermType 'Terminal
    TTMonitor    :: String -> TermType 'Monitor
    TTWindow     :: TermType 'Window

instance Show (TermType any) where 
    show TTTerminal = "term."
    show (TTMonitor name) = name <> "."
    show TTWindow = "window."

callTermFunc :: TermType any -> String -> [String] -> Turtle ()
callTermFunc device name = callFunc $ show device <> name

callTermFuncE :: TermType any -> String -> Turtle ()
callTermFuncE device name = callTermFunc device name []

-- | Get a reference to the terminal.
wrapTerm :: Turtle (TermType 'Terminal)
wrapTerm = return TTTerminal

-- | Get a reference to a peripheral (only monitors supported so far).
wrapPeripheral :: String -> MonitorPos -> Turtle (TermType 'Monitor)
wrapPeripheral name pos = do 
    tell   $ "local " <> name <> " = peripheral.wrap" <> toParams [show $ show pos] <> "\n"
    return $ TTMonitor name

-- | Writes text to the screen, using the current text and background colors.
write :: StringyTVal string a' => TermType any -> string -> Turtle ()
write device text = callTermFunc device "write" [showTVal text]

-- | Writes text to the screen using the specified text and background colors.
blit :: (StringyTVal string1 a', StringyTVal string2 b', StringyTVal string3 c')
     => TermType any -> string1 -> string2 -> string3 -> Turtle () 
blit device text textColors bgColors = callTermFunc device "blit" params
        where params = [showTVal text, showTVal textColors, showTVal bgColors] 

-- | Clears the entire screen.
clear :: TermType any -> Turtle ()
clear = flip callTermFuncE "clear"

-- | Clearns the line the cursor is on.
clearLine :: TermType any -> Turtle ()
clearLine = flip callTermFuncE "clearLine"

-- | Returns two arguments containing the x and the y position of the cursor.
getCursorPos :: TermType any -> (String, String) -> Turtle (TVal TurtleVar, TVal TurtleVar)
getCursorPos device (x, y) = do 
    tell $ "local " <> x <> ", " <> y <> " = " <> funcName
    return (TTVar x, TTVar y)
        where funcName = show device <> "getCursorPos()\n"

-- | Sets the cursor's position.
setCursorPos :: (NumericTVal num1 a', NumericTVal num2 b') => TermType any -> (num1, num2) -> Turtle ()
setCursorPos device (showTVal -> x, showTVal -> y) = do 
    callTermFunc device "setCursorPos" [x, y]

-- | Disables the blinking or turns it on.
setCursorBlink :: TruthyTVal num a' => TermType any -> num -> Turtle ()
setCursorBlink device (showTVal -> newState) = callTermFunc device "setCursorBlink" [newState]
    
-- | Returns whether the terminal supports color.
isColor :: TermType any -> TVal Bool
isColor device = TBool $ show device <> "isColor()"

-- | Returns two arguments containing the x and y values stateing the size of the screen. 
-- Good for if you're making something to be compatible with both Turtles and Computers.
getSize :: TermType any -> (String, String) -> Turtle (TVal TurtleVar, TVal TurtleVar)
getSize device (width, height) = do 
    tell $ "local " <> width <> ", " <> height <> " = " <> funcName
    return (TTVar width, TTVar height)
        where funcName = show device <> "getSize()\n"

-- | Scrolls the terminal n times
scroll :: NumericTVal num a' => TermType any -> num -> Turtle ()
scroll device (showTVal -> n) = callTermFunc device "scroll" [n]

-- TODO: Add redirect
-- TODO: Add current 
-- TODO: Add native 

-- | Sets the text color fo the terminal.
-- Limited functionality without an advanced Computer / Turtle / Monitor
setTextColor :: NumericTVal num a' => TermType any -> num -> Turtle ()
setTextColor device (showTVal -> color) = callTermFunc device "setTextColor" [color]

-- | Returns the current text color of the terminal.
getTextColor :: TermType any -> TVal Number
getTextColor device = TNumber $ show device <> "getTextColor()"

-- | Sets the background color of the terminal.
-- Limited functionality without an advanced Computer / Turtle / Monitor
setBackgroundColor :: NumericTVal num a' => TermType any -> num -> Turtle ()
setBackgroundColor device (showTVal -> color) = callTermFunc device "setBackgroundColor" [color]

-- | Returns the current background color of the terminal 
getBackgroundColor :: TermType any -> TVal Number 
getBackgroundColor device = TNumber $ show device <> "getBackgroundColor()"

-- | Sets the text scale. 
-- Available only to Monitor Objects.
setTextScale :: NumericTVal num a' => TermType 'Monitor -> num -> Turtle ()
setTextScale device (showTVal -> scale) = callTermFunc device "setTextScale" [scale]

-- | Determines whether subsequent renders to the window will be visible.
-- Available only to Window Objects.
setVisible :: TruthyTVal bool a' => TermType 'Window -> bool -> Turtle ()
setVisible device (showTVal -> newState) = callTermFunc device "setVisible" [newState]

-- | Redraws the contents of the window.
-- Available only to Window Objects.
redraw :: TermType 'Window -> Turtle ()
redraw = flip callTermFuncE "redraw"

-- | Returns the cursor back to its position / state within the window.
-- Available only to Window Objects.
restoreCursor :: TermType 'Window -> Turtle ()
restoreCursor = flip callTermFuncE "restoreCursor"

-- | Returns the top left coordinate of the window.
-- Available only to Window Objects.
getPosition :: TermType 'Window -> (String, String) -> Turtle (TVal TurtleVar, TVal TurtleVar)
getPosition device (x, y) = do 
    tell $ "local " <> x <> ", " <> y <> " = " <> funcName
    return (TTVar x, TTVar y)
        where funcName = show device <> "getPosition()\n"

-- | Moves the window.
-- Available only to Window Objects.
reposition :: (NumericTVal num1 a', NumericTVal num2 b') 
           => TermType 'Window -> (num1, num2) -> Turtle ()
reposition device (showTVal -> x, showTVal -> y) = callTermFunc device "reposition" [x, y]

-- | Moves and resizes the window.
-- Available only to Window Objects.
repositionWH :: ( NumericTVal num1 a'
                , NumericTVal num2 b'
                , NumericTVal num3 c'
                , NumericTVal num4 d'
                )
             => TermType 'Window -> (num1, num2) -> (num3, num4) -> Turtle ()
repositionWH device (showTVal -> x, showTVal -> y) (showTVal -> width, showTVal -> height) = do 
    callTermFunc device "reposition" [x, y, width, height]
