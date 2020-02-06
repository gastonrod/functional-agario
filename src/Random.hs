module Random
(
  Seed,
  randomSeeds,
  randDoubleInRange
)
where 
import System.Random
import Data.Time
import Data.Time.Clock.POSIX

type Seed = Int

randDoubleInRange :: Seed -> Double -> Double -> Double
randDoubleInRange seed lo hi = fst(randomR (lo, hi :: Double) (mkStdGen seed))

randIntList :: Seed -> [Int]
randIntList seed = randoms (mkStdGen seed) :: [Int]

randomSeeds :: Seed -> [Seed]
randomSeeds seed = (randIntList seed) :: [Seed]

