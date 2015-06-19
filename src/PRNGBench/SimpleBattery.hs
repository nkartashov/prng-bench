module PRNGBench.SimpleBattery
 where

import System.Random (RandomGen, next, split, getStdGen)
import Criterion.Main
import Criterion.Types

nextStreamFromGen :: RandomGen g => g -> [Int]
nextStreamFromGen gen = map fst $ iterate wrappedNext firstStreamElement
  where firstStreamElement = next gen

wrappedNext :: RandomGen g => (Int, g) -> (Int, g)
wrappedNext (_, newGen) = next newGen

manyRandomsFromNext :: RandomGen g => g -> Int -> [Int]
manyRandomsFromNext gen n = take n $ nextStreamFromGen gen

randomLengths = [100, 1000, 10000, 100000, 1000000, 10000000]

benchFromRandomLength :: RandomGen g => g -> Int -> Benchmark
benchFromRandomLength gen n = bench (show n ++ " random numbers") $ nf (manyRandomsFromNext gen) n

manyRandomsBenchGroup :: RandomGen g => g -> Benchmark
manyRandomsBenchGroup gen = bgroup "Generate many random numbers" $ map (benchFromRandomLength gen) randomLengths
