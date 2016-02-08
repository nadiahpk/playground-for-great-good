
# Types and type classes

## Types

* to get a thing's type, `:t thing`, and it will return `thing :: type`, where `::` is read as "is of type"
``` haskell
ghci> :t rightTriangles -- from example before
rightTriangles :: [(Integer, Integer, Integer)]
```

* specify type on function
 * see example in `funcs.hs`, `removeNonUppercase :: [Char] -> [Char]`
 ```
 ghci> :l funcs.hs
 [1 of 1] Compiling Main             ( funcs.hs, interpreted )
 Ok, modules loaded: Main.
 ghci> removeNonUppercase "asdfsdfks;kdj;jJL:DLIs"
 "JLDLI"
 ```
 * can't figure out how to do that in interactive mode though

* `a` is for any type, e.g. head takes a list of any type and returns the first element, which will be of that type
```
ghci> :t head
head :: [a] -> a
```

## Type classes

* things before `=>` are the class constraint, meaning the function can only be applied to members of this class
``` haskell
ghci> :t (==)
(==) :: Eq a => a -> a -> Bool
-- Eq - types that support equality testing
ghci> :t (>)
(>) :: Ord a => a -> a -> Bool
-- Ord - can be ordered
ghci> :t show
show :: Show a => a -> String
-- Show - can be turned into a string
```
* `read` is interesting, because when we check it we see that it doesn't know what type to return
```haskell
ghci> :t read
read :: Read a => String -> a
-- see below
ghci> :t succ
succ :: Enum a => a -> a
-- Enum - things for which there is a successor and predecessor
```
 so if we just use it on a string, it will spit. But we can solve this by using an explicit type annotation
 ```haskell
ghci> read "5"

<interactive>:16:1:
    No instance for (Read a0) arising from a use of `read'
    The type variable `a0' is ambiguous
    Possible fix: add a type signature that fixes these type variable(s)
    Note: there are several potential instances:
      instance Read () -- Defined in `GHC.Read'
      instance (Read a, Read b) => Read (a, b) -- Defined in `GHC.Read'
      instance (Read a, Read b, Read c) => Read (a, b, c)
        -- Defined in `GHC.Read'
      ...plus 25 others
    In the expression: read "5"
    In an equation for `it': it = read "5"
ghci> read "5" :: Int
5
 ```

NEXT: do these exercises maybe https://github.com/noelmarkham/learn-you-a-haskell-exercises
