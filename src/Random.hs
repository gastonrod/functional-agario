module Random
(
  Seed,
  randomSeeds,
  randIntListLimitRange,
  randDoubleInRange
)
where 
import System.Random
import Data.Time
import Data.Time.Clock.POSIX

type Seed = Int

randIntInRange :: Int -> Int
randIntInRange maxValue = mod ((randIntList 2)!!0) maxValue

randDoubleInRange :: Seed -> Double -> Double -> Double
randDoubleInRange seed lo hi = fst(randomR (lo, hi :: Double) (mkStdGen seed))

randIntListLimitRange :: Seed -> Int -> Int -> Int -> [Int]
randIntListLimitRange seed limit lo hi = map (\x -> (mod x (hi-lo))+lo) (randIntListLimit seed limit)

randIntListLimit :: Seed -> Int -> [Int]
randIntListLimit seed limit = take limit (randIntList seed)

randIntList :: Seed -> [Int]
randIntList seed = randoms (mkStdGen seed) :: [Int]

randomSeeds :: Seed -> [Seed]
randomSeeds seed = (randIntList seed) :: [Seed]

