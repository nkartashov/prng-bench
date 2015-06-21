module PRNGBench
 where

import System.Random (RandomGen)

import Criterion.Main (Benchmark, defaultMain, bgroup)

import PRNGBench.SimpleBattery

benchGroups :: RandomGen g => [g -> Benchmark]
benchGroups = [manyRandomsBenchGroup, manySplitsBenchGroup]

genToBenchGroup :: RandomGen g => String -> g -> Benchmark
genToBenchGroup name gen = bgroup name $ map (\b -> b gen) benchGroups

runGroups :: RandomGen g => String -> g -> IO ()
runGroups name = defaultMain . (:[]) . genToBenchGroup name
