-- chapter 8 learn you haskell

module Ch8 where

import Control.Monad.HT ((<=<), )

-- http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

-- ghci> [1..3]
-- [1,2,3]
-- ghci> map (*2) [1..3]
-- [2,4,6]
-- ghci> fmap (*2) [1..3]
-- [2,4,6]
-- ghci> map (*2) Nothing
--   ERROR
-- ghci> fmap (*2) Nothing
-- Nothing
-- 
-- fmap :: (a -> b) -> fa -> fb
-- fmap takes a function (a -> b), like (*2), and a functor
-- fa, like Nothing, and creates a new functor fb, like
-- Nothing

-- functions are also functors:
-- ghci> let ff = fmap (+2) (+3) -- let ff = (+3) . (+2)
-- ghci> ff 2
-- 7

-- https://en.wikibooks.org/wiki/Haskell/Applicative_functors

-- fmap has an infix synonymn the dollar sign,
-- ghci> (*2) <$> [1..3] -- so the list counts as a "wrapped value"?
-- [2,4,6]
-- ghci> (*2) <$> Nothing
-- Nothing
-- some analogous thing happens with applicatives (?) and
-- the <*> sign

-- applicative f (a -> b) -> f a -> f b
-- TODO: explain this in words, 
-- <*> takes a ... ? function inside a functor ? f(a->b), like Just (+2),
-- and a functor fa, like Nothing, and creates a new functor fb, like
-- Nothing

-- How would you sum Just 2 and Just 3?
-- ghci> (+2) <$> Just 3
-- Just 5
-- ghci> Just (+2) <$> Just 3
--   ERROR
-- ghci> let ff = (+) <$> (Just 2)
--  this gives me Just (+2) but how to show it TODO
-- ghci> ff <*> Just 3
-- Just 5
-- ghci> Just (+2) <*> Just 3
-- Just 5

-- Sequencing 
-- ghci> [(2*),(5*)] <*> [1,4]
-- [2,8,5,20]
-- The *> and <* differ by their sequencing effect .. can't
-- get example working TODO -
-- https://en.wikibooks.org/wiki/Haskell/Applicative_functors#Sequencing_of_effects

-- back to here: http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
-- $: functor, applies function to wrapped value
-- *: applicative, applies wrapped function to wrapped value
-- monads are about applying "a function that returns a wrapped value to a wrapped value"

-- this function returns a wrapped value
half :: Integral a => a -> Maybe a -- Integral refers to integer-like things, things you could apply `even` to
half x = if even x
    then Just (x `div` 2)
    else Nothing

-- ghci> half 2
-- Just 1
-- ghci> half (Just 2)
--   ERROR
-- ghci> Just 2 >>= half
-- Just 1
-- 
--     (>>=) :: m a -> (a -> m b) -> m b
--     takes a monad m a, like Just 2, a function that returns a monad (a -> mb), like half, and returns a monad m b, like Just 1
--
-- ghci> half 8
-- Just 4
-- ghci> half 8 >>= half
-- Just 2
-- [import Control.Monad.HT above]
-- ghci> half <=< half $ 8
-- Just 2
-- [compare to half . half $ 8]



-- ===================================================
-- GOAL: composeM and iterateM are from prob package.
-- Understand what they mean.
-- ===================================================

composeM :: Monad m => [a -> m a] -> a -> m a
composeM = foldl (flip (<=<)) return
-- composeM = foldl (>=>) return -- Told me to try this but didn't work

iterateM :: Monad m => Int -> (a -> m a) -> (a -> m a)
iterateM n f = composeM $ replicate n f


-- ghci> let ff = iterateM 2 half
-- ghci> ff 8
-- Just 2
--
-- ghci> let ff = replicate 3 (+5)
-- ghci> ff <*> [1..3]
-- [6,7,8,6,7,8,6,7,8]
--
-- ghci> let ff = replicate 2 half
-- ghci> ff <*> [1..2]
-- [Nothing,Just 1,Nothing,Just 1]
--
-- so evidently ff = [(half), (half)]
--
-- ghci> let gg = composeM $ replicate 2 half
-- ghci> :t gg
-- gg :: Integral a => a -> Maybe a
-- ghci> gg 1
-- Nothing
-- ghci> gg 2
-- Nothing
-- ghci> gg 7
-- Nothing
-- ghci> gg 8
-- Just 2
--
-- so composeM converts [(half), (half)] into half <=< half 

third :: Integral a => a -> Maybe a 
third x = if (x `mod` 3) == 0
    then Just (x `div` 3)
    else Nothing

by2 :: Integral a => a -> Maybe a -- Integral refers to integer-like things, things you could apply `even` to
by2 x = if x > 10
    then Nothing
    else Just (x*2)

-- ghci> third 6
-- Just 2
-- ghci> third 5
-- Nothing

-- ghci> composeM [(half), (by2)] 1
-- Nothing
-- ghci> composeM [(by2), half] 1
-- Just 1

compose2 :: Monad m => [a -> m a] -> a -> m a
compose2 = foldl (<=<) return

-- ghci> compose2 [(half), (by2)] 1
-- Just 1
-- ghci> compose2 [(by2), half] 1
-- Nothing
--
-- So it was the opposite, without the flip it starts at the rhs

{-
-- make more obvious what's going on? something like:
-- compose fs v = foldl (flip (.)) id fs $ v
-- compose3 :: Monad m => [a -> m a] -> a -> m a
-- ghci> half <=< half $ 8
compose3 fs v = foldl (<=<) id fs <$> v -- didn't work TODO
-}

-- why l not r?
compose4 :: Monad m => [a -> m a] -> a -> m a
compose4 = foldr (<=<) return
-- ghci> compose3 [(half), (by2)] 1
-- Just 1
-- ghci> compose3 [(by2), half] 1
-- Nothing
--
-- so what's the differnce? 
-- http://stackoverflow.com/questions/384797/implications-of-foldr-vs-foldl-or-foldl
-- https://wiki.haskell.org/Fold
-- main bit is that right folds can potentially work on infinite lists

-- TODO: what is it meant to be?
-- ghci> foldr (<=<) [(half), (by2)] 1
-- ghci> foldr (<=<) 1 [(half), (by2)] 
--   ERROR for both


-- Ordinary folding
-- ----------------

-- ghci> foldr (-) 54 [10,11]
-- 53
-- ghci> foldl (-) 54 [10,11]
-- 33
--
-- foldr:
--
-- [10,11] == (10:(11:[]))
--
-- foldr (-) 54 ( 10 : ( 11 : [] ) )
--              ( 10 - ( 11 - 54 ) ) -- trick to remember, replace [] with value and : with operator
--              ( 10 - (   -43   ) )
--              (      53          )
-- foldl        (  (54 - 10) - 11  )
--
-- if foldr f x ys where ys = [y1,y2,...,yk] 
-- foldr:   f y1 (f y2 (... (f yk x) ...))
-- foldl:   f (... (f (f x y1) y2) ...) yk


-- ghci> let ff = foldr (<=<) id [(half), (by2)]
-- ghci> ff 1
--   ERROR
-- ghci> half <=< by2 $ 1
-- Just 1
-- ghci> ff $ 1
--   ERROR :(
-- ghci> :t ff
-- ff :: Integral b => Maybe b -> Maybe b 

-- ha. `return` is `id` for ... these whatever-they-ares
-- let ff = foldr (<=<) return [(half), (by2)]
-- ghci> ff 1
-- Just 1

-- NEXT -- figure out why they used foldl and flip etc.
