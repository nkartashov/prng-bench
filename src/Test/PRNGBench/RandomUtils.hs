module Test.PRNGBench.RandomUtils
  where

import System.Random (RandomGen, next, split)
import Data.Bits
import Data.List

nextStreamFromGen :: RandomGen g => g -> [Int]
nextStreamFromGen gen = map fst $ iterate wrappedNext firstStreamElement
  where firstStreamElement = next gen

nextInt :: RandomGen g => g -> Int
nextInt gen = let (int, _) = next gen in int

wrappedNext :: RandomGen g => (Int, g) -> (Int, g)
wrappedNext (_, newGen) = next newGen

isPowerOfTwo :: Int -> Bool
isPowerOfTwo n | n <= 0 = False
               | otherwise = popCount n == 1

splitToList :: RandomGen g => g -> [g]
splitToList gen = let (g1, g2) = split gen in [g1, g2]

splitToPowerOfTwo :: RandomGen g => Int -> g -> [g]
splitToPowerOfTwo n gen | n == 1 = [gen]
                              | otherwise = concatMap (splitToPowerOfTwo $ n `div` 2) $ splitToList gen

nextPowerOfTwo :: Int -> Int
nextPowerOfTwo n | isPowerOfTwo n = n
                 | otherwise = (2^) $ (1+) $ sum $ unfoldr (\n -> if n <= 1 then Nothing
                                                                            else Just (1, n `div` 2)) n

splitToAnyNumber :: RandomGen g => Int -> g -> [g]
splitToAnyNumber n = take n . splitToPowerOfTwo (nextPowerOfTwo n)

type NextCount = Int
type SplitCount = Int

nextStreamFromSplitGenerators :: RandomGen g => NextCount -> g -> [[Int]]
nextStreamFromSplitGenerators = (map nextStreamFromGen .) . splitToAnyNumber

nextFromSplitGenerators :: RandomGen g => NextCount -> SplitCount -> g -> [[Int]]
nextFromSplitGenerators n splits = map (take n) . nextStreamFromSplitGenerators splits
