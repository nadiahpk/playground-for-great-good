-- Erwig and Kollmansberger (2006) Section 3

module Tree where

import qualified Numeric.Probability.Transition as Trans
import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Percentage
    (Dist, Trans, RTrans, Expand, RExpand, Space, ) -- If I don't include this, it gives error "Not in scope: type constructor or class ‘Trans’"

-- import qualified Numeric.Probability.Monad as MonadExt -- won't import them, apparently I have to cut and paste it from the thing itself http://stackoverflow.com/questions/14676675/numeric-probability-monad-not-exported-from-probability-package-and-therefore-ex
import Control.Monad.HT ((<=<), )

composeM :: Monad m => [a -> m a] -> a -> m a
composeM = foldl (flip (<=<)) return
-- composeM = foldl (>=>) return -- Told me to try this but didn't work

iterateM :: Monad m => Int -> (a -> m a) -> (a -> m a)
iterateM n f = composeM $ replicate n f
-- end of cut and paste

type Height = Int

-- a new data type called "Tree" can take three values
-- it can be "Alive" and have a height, it can be "Hit" and have a height, or it can be "Fallen"
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses
data Tree = Alive Height | Hit Height | Fallen 
  deriving (Ord,Eq,Show) -- allows printing, operations, etc of 

grow :: Trans Tree 
grow (Alive h) = Dist.normal (map Alive [h+1..h+5])

-- ghci> let seed = Alive 0
-- ghci> seed
-- Alive 0
-- ghci> grow $ seed -- dollar sign makes things after it get evaluated first, e.g. `show (1+1)` is equiv to `show $ 1+1`
-- fromFreqs [(Alive 3, 25.1%),(Alive 2, 22.2%),(Alive 4, 22.2%),(Alive 1, 15.2%),(Alive 5, 15.2%)]

hit :: Trans Tree
hit (Alive h) = Dist.certainly (Hit h)

fall :: Trans Tree
fall _ = Dist.certainly Fallen

evolve :: Trans Tree
-- the at symbol is for pattern matching, it's looking for something that matches Alive _ to apply the rhs to
evolve t@(Alive _) = Trans.unfold (Dist.enum [90,4,6] [grow,hit,fall]) t 
evolve t@(Hit _) = Dist.certainly Fallen 
-- including the above means 
-- ghci> evolve $ Hit 10
-- fromFreqs [(Fallen,100.0%)]
evolve t = Dist.certainly t -- I guess this catches ones that didn't match above cases?
-- The function Trans.unfold converts a distribution of transitions into a regular transition. unfoldT :: Dist (Trans a) -> Trans a

seed :: Tree
seed = Alive 0

tree :: Int -> Tree -> Dist Tree
tree n = iterateM n evolve
-- ghci> tree 1 seed
-- fromFreqs [(Alive 3, 22.6%),(Alive 2, 20.0%),(Alive 4, 20.0%),(Alive 1, 13.7%),(Alive 5, 13.7%),(Fallen,  6.0%),(Hit 0,  4.0%)]
-- ghci> tree 2 seed
-- fromFreqs [(Alive 6, 16.9%),(Fallen, 15.4%),(Alive 5, 14.5%),(Alive 7, 14.5%),(Alive 4, 10.2%),(Alive 8, 10.2%),(Alive 3,  5.5%),(Alive 9,  5.5%),(Alive 2,  1.9%),(Alive 10,  1.9%),(Hit 3,  0.9%),(Hit 2,  0.8%),(Hit 4,  0.8%),(Hit 1,  0.5%),(Hit 5,  0.5%)]


-- aside: figure out what compose and iterate are about
-- ----------------------------------------------------
-- composeM = foldl (flip (<=<)) return
-- first do [ ] chapter 8 learn you haskell
--          [x] this: http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
