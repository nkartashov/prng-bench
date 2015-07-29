module Test.PRNGBench.SimpleBattery
 where

import System.Random (RandomGen)

import Criterion.Main

import Test.PRNGBench.RandomUtils
import Test.PRNGBench.GenList (AnnotatedGenList)

manyRandomsFromNext :: RandomGen g => g -> Int -> [Int]
manyRandomsFromNext gen n = take n $ nextStreamFromGen gen

randomLengths :: [Int]
randomLengths = [1000]

manyRandomsBenchGroup :: AnnotatedGenList -> Benchmark
manyRandomsBenchGroup gens = bgroup "Next" $ do
  len <- randomLengths
  (name, gen) <- gens
  return $ bench (show len ++ "_" ++ name) $ nf (manyRandomsFromNext gen) len

splitNumbers :: [Int]
splitNumbers = [1000, 2048]

nextsOnEachSplit :: [Int]
nextsOnEachSplit = [1]

manySplitsBenchGroup :: AnnotatedGenList -> Benchmark
manySplitsBenchGroup gens = bgroup "SplitNext" $ do
  splits <- splitNumbers
  nexts <- nextsOnEachSplit
  (name, gen) <- gens
  return $ bench (show splits ++ "_" ++ show nexts ++ "_" ++ name) $ nf (nextFromSplitGenerators nexts splits) gen
