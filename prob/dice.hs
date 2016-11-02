import qualified Numeric.Probability.Distribution as Dist
import Control.Monad (liftM2, replicateM)
-- import Control.Monad (liftM2, liftM3, replicateM) 
import Numeric.Probability.Distribution ((??), )

type Probability = Rational
type Dist = Dist.T Probability

-- Making the dice etc.
-- --------------------

die :: Dist Int
die = Dist.uniform [1..4]
-- ghci> die
-- fromFreqs [(1,1 % 4),(2,1 % 4),(3,1 % 4),(4,1 % 4)]

dieX :: Int -> Dist Int
dieX sides = Dist.uniform [1..sides]
-- ghci> dieX 5
-- fromFreqs [(1,1 % 5),(2,1 % 5),(3,1 % 5),(4,1 % 5),(5,1 % 5)]

-- this is a "transition" which, given a number, either adds one or doesn't with equal probability
-- "a transition that is a probabilistic function on just one type.... A common operation for a transition is to apply it to a distribution"
succOrId :: Int -> Dist Int
succOrId x = Dist.uniform [x, x+1]
-- ghci> succOrId 2
-- fromFreqs [(2,1 % 2),(3,1 % 2)]

-- here's another way of achieving the same thing, using their "choose"
succOrId2 :: Int -> Dist Int
succOrId2 x = Dist.choose 0.2 x (x+1) -- note the 0.2 refers to the second event
-- ghci> succOrId2 4
-- fromFreqs [(5,4 % 5),(4,1 % 5)]

-- you can put the roll of the dice distribution into the transition... useful for selection of territory-winner after dispersal?
droll :: Dist Int
droll = do 
    d <- die
    succOrId d
-- ghci> droll
-- fromFreqs [(2,1 % 4),(3,1 % 4),(4,1 % 4),(1,1 % 8),(5,1 % 8)]

-- same as above but easier to read
droll2 :: Dist Int
-- droll2 = die >>= succOrId -- these bits are called "bindings"
droll2 = die >>= succOrId >>= succOrId -- you can chain them apparently, nice

-- QUESTION TODO: how do I make a chain of user-specified arbitrary length? 


-- Querying the dice - it's done with a ??
-- ---------------------------------------

-- somehow, this sorcery gave me the probability of all combined outcomes
twoDice :: Dist (Int,Int)
twoDice = liftM2 (,) die die
-- ghci> twoDice
-- fromFreqs [((1,1),1 % 16),((1,2),1 % 16),((1,3),1 % 16),((1,4),1 % 16),((2,1),1 % 16),((2,2),1 % 16),((2,3),1 % 16),((2,4),1 % 16),((3,1),1 % 16),((3,2),1 % 16),((3,3),1 % 16),((3,4),1 % 16),((4,1),1 % 16),((4,2),1 % 16),((4,3),1 % 16),((4,4),1 % 16)]

-- QUESTION TODO: how would I make a distribution of an arbitrary number of dice?


twoSixes :: Probability
twoSixes = (==(6,6)) ?? twoDice
-- hah, returns me 0 % 1 because I forgot I made a d4

twoFours :: Probability
twoFours = (==(4,4)) ?? twoDice
-- ghci> let a = twoFours
-- ghci> a
-- 1 % 16
-- ghci> a * 2
-- ghci> :t a
-- a :: Probability

greaterThanX :: Int -> Probability
greaterThanX x = (>x) ?? die
-- ghci> greaterThanX 3
-- 1 % 4

-- Something to do with lift
-- ------------------------

-- This adds them ... what else can you do?
addTwo :: Dist Int
addTwo =
   liftM2 (+) die die

multTwo :: Dist Int
multTwo =
   liftM2 (*) die die

{-
addThree :: Dist Int -- didn't work :-(
addThree =
    liftM3 (+) die die die
-}

-- Repeated application
-- --------------------

dice :: Int -> Dist [Int]
dice = flip replicateM die
-- ghci> dice 1
-- fromFreqs [([1],1 % 4),([2],1 % 4),([3],1 % 4),([4],1 % 4)]
-- ghci> dice 2
-- fromFreqs [([1,1],1 % 16),([1,2],1 % 16),([1,3],1 % 16),([1,4],1 % 16),([2,1],1 % 16),([2,2],1 % 16),([2,3],1 % 16),([2,4],1 % 16),([3,1],1 % 16),([3,2],1 % 16),([3,3],1 % 16),([3,4],1 % 16),([4,1],1 % 16),([4,2],1 % 16),([4,3],1 % 16),([4,4],1 % 16)]

-- UP TO HERE: figure out what flip, replicateM mean
