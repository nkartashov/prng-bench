module Test.PRNGBench.RandomUtils
  ( nextStreamFromGen
  , nextFromSplitGenerators
  , splitToAnyNumber
  ) where

import System.Random (RandomGen, next, split)
import Data.Bits ((.&.))

-- | Produces an infinite stream of random numbers given a generator
nextStreamFromGen :: RandomGen g => g -> [Int]
nextStreamFromGen gen = map fst $ iterate wrappedNext firstStreamElement
  where firstStreamElement = next gen

-- | Wrapper for 'next' permitting the use of 'iterate'
wrappedNext :: RandomGen g => (Int, g) -> (Int, g)
wrappedNext (_, newGen) = next newGen

-- | Performs 'split' and wraps the results in a list
splitToList :: RandomGen g => g -> [g]
splitToList gen = let (g1, g2) = split gen in [g1, g2]

-- | Splits the given generator into 'n' generators, 'n' being a power of 2
-- and then wraps them into a list
splitToPowerOfTwo :: RandomGen g => Int -> g -> [g]
splitToPowerOfTwo n gen | n == 1 = [gen]
                        | otherwise = concatMap (splitToPowerOfTwo $ n `div` 2) $ splitToList gen

-- | Finds the next power of 2 for a given n
nextPowerOfTwo :: Int -> Int
nextPowerOfTwo = nextPowerOfTwoPositive 1

-- | Helper function for a 'nextPowerOfTwo'
nextPowerOfTwoPositive :: Int -> Int -> Int
nextPowerOfTwoPositive p n | n > 0 && (n .&. (n - 1)) == 0 = n
                           | p > n = p
                           | otherwise = nextPowerOfTwoPositive (p * 2) n

-- | Splits the generator into 'n' generators, 'n' being a positive number
-- and wraps the results into a list
splitToAnyNumber :: RandomGen g => Int -> g -> [g]
splitToAnyNumber n | n <= 0 = error "Argument for splitting must be positive"
                   | otherwise = take n . splitToPowerOfTwo (nextPowerOfTwo n)

type NextCount = Int
type SplitCount = Int

-- | Splits the given generator into 'n' generators and produces an infinite
-- stream of random numbers from each
nextStreamFromSplitGenerators :: RandomGen g => SplitCount -> g -> [[Int]]
nextStreamFromSplitGenerators = (map nextStreamFromGen .) . splitToAnyNumber

-- | Splits the given generator into 'splits' generators and produces 'n'
-- random numbers from each
nextFromSplitGenerators :: RandomGen g => NextCount -> SplitCount -> g -> [[Int]]
nextFromSplitGenerators n splits = map (take n) . nextStreamFromSplitGenerators splits
