module PRNGBench.SimpleBattery
 where

import System.Random (RandomGen)

import Criterion.Main
import Criterion.Types

import PRNGBench.RandomUtils

manyRandomsFromNext :: RandomGen g => g -> Int -> [Int]
manyRandomsFromNext gen n = take n $ nextStreamFromGen gen

randomLengths = [1000, 100000, 1000000]

benchFromRandomLength :: RandomGen g => g -> Int -> Benchmark
benchFromRandomLength gen n = bench (show n) $ nf (manyRandomsFromNext gen) n

manyRandomsBenchGroup :: RandomGen g => g -> Benchmark
manyRandomsBenchGroup gen = bgroup "Nexts" $ map (benchFromRandomLength gen) randomLengths

splitNumbers = [50, 128, 1000, 2048]
nextsOnEachSplit = [1]

manySplitsBenchGroup :: RandomGen g => g -> Benchmark
manySplitsBenchGroup gen = bgroup "Splits + Nexts" $ do
  splits <- splitNumbers
  nexts <- nextsOnEachSplit
  return $ bench (show splits ++ " splits, " ++ show nexts ++ " nexts") $ nf (nextFromSplitGenerators nexts splits) gen

