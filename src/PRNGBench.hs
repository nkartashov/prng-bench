module PRNGBench
 where

import System.Random (RandomGen)
import Data.List

import Criterion.Main (Benchmark, defaultMain, bgroup)

import PRNGBench.SimpleBattery

benchGroups :: RandomGen g => [g -> Benchmark]
benchGroups = [manyRandomsBenchGroup, manySplitsBenchGroup]

runGroups :: RandomGen g => String -> g -> IO ()
runGroups name gen = defaultMain $ [bgroup name $ map (\b -> b gen) benchGroups]
