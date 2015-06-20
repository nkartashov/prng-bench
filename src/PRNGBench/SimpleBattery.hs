module PRNGBench.SimpleBattery
 where

import System.Random (RandomGen)

import Criterion.Main
import Criterion.Types

import PRNGBench.RandomUtils

manyRandomsFromNext :: RandomGen g => g -> Int -> [Int]
manyRandomsFromNext gen n = take n $ nextStreamFromGen gen

randomLengths = [100, 1000, 10000, 100000, 1000000, 10000000]

benchFromRandomLength :: RandomGen g => g -> Int -> Benchmark
benchFromRandomLength gen n = bench (show n ++ " random numbers") $ nf (manyRandomsFromNext gen) n

manyRandomsBenchGroup :: RandomGen g => g -> Benchmark
manyRandomsBenchGroup gen = bgroup "Generate many random numbers" $ map (benchFromRandomLength gen) randomLengths

splitNumbers = [10, 16, 50, 64, 128, 1000, 2048]
nextsOnEachSplit = [1, 10, 100, 500, 1000]

manySplitsBenchGroup :: RandomGen g => g -> Benchmark
manySplitsBenchGroup gen = bgroup "Make many splits and then nexts" $ do
  splits <- splitNumbers
  nexts <- nextsOnEachSplit
  return $ bench (show splits ++ " splits, " ++ show nexts ++ " nexts") $ nf (nextFromSplitGenerators nexts splits) gen

