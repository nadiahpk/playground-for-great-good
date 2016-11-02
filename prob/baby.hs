import qualified Numeric.Probability.Distribution as Dist

data Outcome = Win | Lose

type Probability = Rational
type Dist a = Dist.T Probability a

firstChoice :: Dist Outcome
firstChoice = Dist.uniform [Win, Lose, Lose]
