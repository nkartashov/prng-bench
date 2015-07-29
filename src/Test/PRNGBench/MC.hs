module Test.PRNGBench.MC where

import System.Random (RandomGen, split, randomRs)

import Criterion.Main

import Test.PRNGBench.RandomUtils (splitToAnyNumber)
import Test.PRNGBench.GenList (AnnotatedGenList)

radius :: Double
radius = 1.0

diameter :: Double
diameter = radius * 2

isInCircle :: (Double, Double) -> Bool
isInCircle (x, y) = x ** 2 + y ** 2 <= radius ** 2

toHit :: (Double, Double) -> Int
toHit coords = if isInCircle coords then 1 else 0

leftmostLimit :: Double
leftmostLimit = -radius

runSlice :: RandomGen g => g -> Int -> (Double, Double) -> Double
runSlice gen dotsInTheSlice (left, right) = (\res -> fromIntegral res / fromIntegral dotsInTheSlice) $ sum hits
  where
    hits = map toHit $ take dotsInTheSlice coordinateStream
    (xGen, yGen) = split gen
    coordinateStream = zip (randomRs (left, right) xGen) $ randomRs (-1, 1) yGen

runCircleMC :: RandomGen g => Int -> Int -> g -> Double
runCircleMC sliceNumber dotsPerSlice gen = sum resultsOnSlices
  where
    resultsOnSlices = map workOnSliceIndex $ zip gens [1..sliceNumber]
    workOnSliceIndex (g, sliceIndex) = runSlice g dotsPerSlice $ sliceNumberToLimits $ fromIntegral sliceIndex
    sliceSize = diameter / fromIntegral sliceNumber
    sliceNumberToLimits i = (leftmostLimit + sliceSize * (i - 1), leftmostLimit + sliceSize * i)
    gens = splitToAnyNumber sliceNumber gen

slices :: [Int]
slices = [10, 100]

dots :: [Int]
dots = [1000, 100000]

runCircleMCBattery :: AnnotatedGenList -> Benchmark
runCircleMCBattery gens = bgroup "MC_Circle" $ do
  sliceInstance <- slices
  dotsInstance <- dots
  (name, gen) <- gens
  return $ bench (show sliceInstance ++ "_" ++ show dotsInstance ++ "_" ++ name)
    $ nf (runCircleMC sliceInstance dotsInstance) gen
