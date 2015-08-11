module Test.PRNGBench.SimpleBattery
  ( manyRandomsBenchGroup
  , manySplitsBenchGroup
  ) where

import System.Random (RandomGen)

import Criterion.Main (Benchmark, bench, bgroup, nf)

import Test.PRNGBench.RandomUtils (nextStreamFromGen, nextFromSplitGenerators)
import Test.PRNGBench.GenList (AnnotatedGenList)

-- | Produces 'n' random numbers from a given generator
manyRandomsFromNext :: RandomGen g => g -> Int -> [Int]
manyRandomsFromNext gen n = take n $ nextStreamFromGen gen

randomLengths :: [Int]
randomLengths = [1000]

-- | Performs tests in the form of generating some random numbers with 'next'
-- measuring time needed for that
manyRandomsBenchGroup :: AnnotatedGenList -> Benchmark
manyRandomsBenchGroup gens = bgroup "Next" $ do
  len <- randomLengths
  (name, gen) <- gens
  return $ bench (show len ++ "_" ++ name) $ nf (manyRandomsFromNext gen) len

splitNumbers :: [Int]
splitNumbers = [1000, 2048]

nextsOnEachSplit :: [Int]
nextsOnEachSplit = [1]

-- | Performs tests in the form of generating splitting a generator and then generating
-- some random numbers with 'next' measuring time needed for that
manySplitsBenchGroup :: AnnotatedGenList -> Benchmark
manySplitsBenchGroup gens = bgroup "SplitNext" $ do
  splits <- splitNumbers
  nexts <- nextsOnEachSplit
  (name, gen) <- gens
  return $ bench (show splits ++ "_" ++ show nexts ++ "_" ++ name) $ nf (nextFromSplitGenerators nexts splits) gen
