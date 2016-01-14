
# Prompt

* `ghci`, then :set prompt "ghci> "

## Basic syntax

* neq is /=
* `succ 8` gives `9`, but `succ "a"` spits chips
* note absence of a bracket, e.g. `min 9 10`
 * but need brackets for functions in succession, like `succ (succ 8)`
 * `min 9 10 11` spits, so maybe `min 9 (min 10 11)`

# Function files

* `:l baby`
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
```

*UP TO DROP*
