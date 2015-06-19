module PRNGBench
 where

import System.Random (RandomGen)

import Criterion.Main (Benchmark, defaultMain)

import PRNGBench.SimpleBattery


benchGroups :: RandomGen g => [g -> Benchmark]
benchGroups = [manyRandomsBenchGroup]

runGroups :: RandomGen g => g -> IO ()
runGroups gen = defaultMain $ map (\b -> b gen) benchGroups

main :: IO ()
main = undefined -- mapM_ runGroups []
