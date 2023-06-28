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

One can even take advantage of Haskell's syntax:

```haskell
prog :: Turtle ()
prog = do 
    refuel () 

    tWhile detectDown $ do 
        dig () >> digDown () >> down >> dig () >> forward >> turnLeft
```

And to translate the lua code and save it to a file:

```haskell
main :: IO ()
main = writeProgram prog "prog.lua"
```
