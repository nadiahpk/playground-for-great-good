
# Prompt

* `ghci`, then :set prompt "ghci> "

## Basic syntax

* neq is /=
* `succ 8` gives `9`, but `succ "a"` spits chips, while `succ 'a'` works fine.
* note absence of a bracket, e.g. `min 9 10`
 * but need brackets for functions in succession, like `succ (succ 8)`
 * `min 9 10 11` spits, so maybe `min 9 (min 10 11)`

## What does `succ` do?

Check its type, and you see the `Enum` typeclass:

    Prelude> :t succ
    succ :: Enum a => a -> a

Then ask about `Enum` using `:info`:

    Prelude> :info Enum
    class Enum a where
      succ :: a -> a
      pred :: a -> a
      toEnum :: Int -> a
      fromEnum :: a -> Int
      enumFrom :: a -> [a]
      enumFromThen :: a -> a -> [a]
      enumFromTo :: a -> a -> [a]
      enumFromThenTo :: a -> a -> a -> [a]
  	    -- Defined in ‘GHC.Enum’
    instance Enum Word -- Defined in ‘GHC.Enum’
    instance Enum Ordering -- Defined in ‘GHC.Enum’
    instance Enum Integer -- Defined in ‘GHC.Enum’
    instance Enum Int -- Defined in ‘GHC.Enum’
    instance Enum Char -- Defined in ‘GHC.Enum’
    instance Enum Bool -- Defined in ‘GHC.Enum’
    instance Enum () -- Defined in ‘GHC.Enum’
    instance Enum Float -- Defined in ‘GHC.Float’
    instance Enum Double -- Defined in ‘GHC.Float’

So you can see the other things that work on an `Enum`, like `pred`:

    Prelude> pred 'b'
    'a'

# Function files

* `:l baby`
* Conveniently, `:r` reloads the current module/file.
* the `else` is mandatory, every expression must return something, 

# Lists

* can't just go `a=1` in the prompt, have to go `let a=1`
* can't mix types in lists (e.g. strings and nos)

## Putting them together

* `++` to cat strings and other lists together
* also `:`, which you use to put a single thing on the front of a list, particularly useful for long lists so it doesn't have to walk to the end
 * e.g. `5:[1,2,3]`
 * `[1,2,3]` is actually just syntactic sugar for `1:2:3:[]`

## Indexing

* index with double exclaim
```haskell
ghci> let lostNumbers = [4,8,15,16,23,42]  
ghci> lostNumbers !! 2
15
```
* Also can use `head` (first element), `tail` (all but first), `init` (all but last) and `last lostNumbers`
 * can't use any of these on an empty list, `null lostNumbers` checks isempty

## Comparisons

* errr...
```haskell
ghci> [3,2,1] > [2,10,100]
```
 * read it like it's the dictionary ordering! if 100 were the 100th letter of the alphabet, etc
* *BUT* if one is shorter than the other, do the *opposite* of what a dictionary would!
```haskell
ghci> [3,4,2] > [3,4]
True
ghci> "a" > "and"
False
```

## Manipulate

* `take` gets you that many of the list, from the front
 * `take` doesn't change the list itself!
```haskell
ghci> lostNumbers
[4,8,15,16,23,42]
ghci> take 3 lostNumbers
[4,8,15]
ghci> lostNumbers
[4,8,15,16,23,42]
ghci> take 0 lostNumbers 
[]
ghci> let c = take 3 lostNumbers 
ghci> c
[4,8,15]
```
* `drop` is like `take`, but from the end
```
ghci> drop 3 lostNumbers 
[16,23,42]
```
* other things that'll do what you expect: `maximum`, `minimum`, `sum`, `product`
* `elem` checks if it's an element of
```
ghci> elem 4 lostNumbers 
True
ghci> 4 `elem` lostNumbers -- as an infix function, apparently easier to read
True 
```

## Create

* in a range:
```
hci> let r = [1..20]
ghci> r
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
ghci> let r = ['b'..'j']
ghci> r
"bcdefghij"
```
* specifying increment:
```
ghci> [1,4..20]
[1,4,7,10,13,16,19]
```
* woah. you can specify an infinite list, and only take the bit that you need from it, and Haskell won't generate the whole list for you
```
ghci> take 5 [13,26..]
[13,26,39,52,65]
ghci> [13,26..]
-- actually tries to generate the whole list for you 
```
* more infinite lists
 * `cycle`
```
ghci> take 9 (cycle "lol ")
"lol lol l"
ghci> drop 9 (cycle "lol ")
-- this doesn't work though
```
 * `repeat`, like cycle with one element
```
ghci> take 3 (repeat 12)
[12,12,12]
```

## List comprehensions

* Uses the syntax from maths
 * Some examples
```
ghci> [x*2 | x <- [1..5]]
[2,4,6,8,10]
ghci> [x*2 | x <- [1..5], x*2 >= 8] -- note condition has to go after the thing that gets x
[8,10]
ghci> [x*2 | x <- [1..5], x*2 >= 8, x*2 < 10]
[8]
```
 * All the numbers up to 100 that are divisible by 24
```
ghci> mod 52 7 -- Recall how mod works
3
ghci> [x | x<-[0..100], mod x 24 == 0]
[0,24,48,72,96]
```
 * Two lists used, does all combinations, with the later list cycled through first. Note the ordering on these.
```
ghci> [[x,y] | x<-[0..2], y<-[10..12]]
[[0,10],[0,11],[0,12],[1,10],[1,11],[1,12],[2,10],[2,11],[2,12]]
ghci> [[x,y] | y<-[10..12], x<-[0..2]]
[[0,10],[1,10],[2,10],[0,11],[1,11],[2,11],[0,12],[1,12],[2,12]]
```
 * Use an underscore for variables that are unused... (why would you want this?)
```
length' xs = sum [1 | _ <- xs]   
```
 * Can nest comprehensions
```
ghci> let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
ghci> [ [ x | x <- xs, even x ] | xs <- xxs]  
[[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]  
```

*UP TO TUPLES*
