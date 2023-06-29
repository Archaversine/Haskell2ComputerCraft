# Haskell to ComputerCraft

Library to convert Haskell code to Lua code for Minecraft's ComputerCraft Mod.

## Examples

Here is an example of Lua code that one may write:

```lua
turtle.refuel()
while turtle.detectDown do 
    turtle.dig()
    turtle.digDown()
    turtle.down()
    turtle.dig()
    turtle.forward()
    turtle.turnLeft()
end
```

The Haskell code would look like:

```haskell
prog :: Turtle ()
prog = do 
    refuel ()

    tWhile detectDown $ do 
        digM ()
        digDownM () 
        moveDownM 
        digM () 
        moveForwardM
        turnLeftM
```

### Local Variables

Local variables are treated as binding monadic actions.

```haskell
prog :: Turtle ()
prog = do 
    refuel () 

    -- Define a local variable x
    x <- defineLocal "x" 0.0

    -- Define a function that digs after a certain action
    let digAfter a = a >> digM ()

    -- Dig while block is detected under
    tWhile detectDown $ do 
        -- Increment x
        -- Custom operator defined in Turtle.Var
        x += 1.0

        -- Dig Actions
        digM () >> digDownM () >> digAfter moveDownM >> moveForwardM >> turnLeftM

    -- Print the value of x
    tPrint x
```

### Control Flow

An example using if else, for loops, while loops, and functions:

```haskell 
-- Function to add two numbers together
add a b = a .+ b

prog :: Turtle ()
prog = do 
    x <- defineLocal "x" 5.0

    tIfElse (x % 2.0 .== 0.0) $ do 
        tPrint "X is even!"
    tElse $ do 
        tPrint "X is odd!"

    mySum <- defineLocal "sum" 0.0

    forFromToStep "i" (1.0, 10.0, 1.0) $ \i -> do 
        mySum += i

    tWhile (mySum .> 0.0) $ do 
        mySum -= 1.0
        tPrint "Removed 1!"

    y <- defineLocal "y" 1.0 
    z <- defineLocal "z" 1.0

    result <- defineLocal "result" $ add y z

    tPrint result
```

### Translating to Lua

And to translate the lua code and save it to a file:

```haskell
main :: IO ()
main = writeProgram prog "prog.lua"
```
