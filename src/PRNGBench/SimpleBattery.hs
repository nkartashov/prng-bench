module PRNGBench.SimpleBattery
 where

import System.Random (RandomGen)

import Criterion.Main
import Criterion.Types()

import PRNGBench.RandomUtils

manyRandomsFromNext :: RandomGen g => g -> Int -> [Int]
manyRandomsFromNext gen n = take n $ nextStreamFromGen gen

randomLengths :: [Int]
randomLengths = [1000]

benchFromRandomLength :: RandomGen g => g -> Int -> Benchmark
benchFromRandomLength gen n = bench (show n) $ nf (manyRandomsFromNext gen) n

manyRandomsBenchGroup :: RandomGen g => g -> Benchmark
manyRandomsBenchGroup gen = bgroup "Next" $ map (benchFromRandomLength gen) randomLengths

splitNumbers :: [Int]
splitNumbers = [1000, 2048]

nextsOnEachSplit :: [Int]
nextsOnEachSplit = [1]

manySplitsBenchGroup :: RandomGen g => g -> Benchmark
manySplitsBenchGroup gen = bgroup "SplitNext" $ do
  splits <- splitNumbers
  nexts <- nextsOnEachSplit
  return $ bench (show splits ++ "_" ++ show nexts) $ nf (nextFromSplitGenerators nexts splits) gen

