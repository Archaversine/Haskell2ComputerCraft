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
        dig ()
        digDown () 
        down 
        dig () 
        forward
        turnLeft
```

Another example with local variables:

```haskell
prog :: Turtle ()
prog = do 
    refuel () 

    -- Define a local variable x
    x <- defineLocal "x" 0.0

    -- Define a function that digs after a certain action
    let digAfter a = a >> dig ()

    -- Dig while block is detected under
    tWhile detectDown $ do 
        -- Increment x
        -- Custom operator defined in Turtle.Var
        x += 1.0

        -- Dig Actions
        dig () >> digDown () >> digAfter down >> forward >> turnLeft

    -- Print the value of x
    tPrint x
```

And to translate the lua code and save it to a file:

```haskell
main :: IO ()
main = writeProgram prog "prog.lua"
```
