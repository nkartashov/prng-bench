module Test.PRNGBench
 where

import System.Random (RandomGen)
import Control.Arrow (second)

import Criterion.Main (Benchmark, defaultMain)

import Test.PRNGBench.GenList (SomeGen(SG), AnnotatedGen, AnnotatedGenList)

import Test.PRNGBench.SimpleBattery (manyRandomsBenchGroup, manySplitsBenchGroup)
import Test.PRNGBench.MC (runSequentialCircleMCBattery, runParallelCircleMCBattery)

-- | A list of default benchmark groups
benchGroups :: [AnnotatedGenList -> Benchmark]
benchGroups = [ manyRandomsBenchGroup
              , manySplitsBenchGroup
              , runSequentialCircleMCBattery
              , runParallelCircleMCBattery
              ]

-- | Given a name and a generator, wraps the latter in an annotated box
genToAnnotatedGen :: RandomGen g => String -> g -> AnnotatedGen
genToAnnotatedGen = curry $ second SG

-- | Runs all the default benchmark groups on a given list of annotated boxed generators
runGroups :: AnnotatedGenList -> IO ()
runGroups gens = defaultMain $ map (\b -> b gens) benchGroups
