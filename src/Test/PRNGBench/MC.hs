module Test.PRNGBench.MC
  (runCircleMCBattery
  )where

import System.Random (RandomGen, split, randomRs)

import Criterion.Main (Benchmark, bench, bgroup, nf)

import Test.PRNGBench.RandomUtils (splitToAnyNumber)
import Test.PRNGBench.GenList (AnnotatedGenList)

radius :: Double
radius = 1.0

diameter :: Double
diameter = radius * 2

-- | Checks if the point is within the unit circle
isInCircle :: (Double, Double) -> Bool
isInCircle (x, y) = x ** 2 + y ** 2 <= radius ** 2

-- | Turns point into 1 if it is within the circle
-- to zero otherwise
toHit :: (Double, Double) -> Int
toHit coords | isInCircle coords = 1
             | otherwise = 0

-- | Leftmost border of the unit circle
leftmostLimit :: Double
leftmostLimit = -radius

-- | Runs MC process on the given slice, throwing dots in it and calculating
-- fraction of them inside the unit circle
runSlice :: RandomGen g => g -> Int -> (Double, Double) -> Double
runSlice gen dotsInTheSlice (left, right) = (\res -> fromIntegral res / fromIntegral dotsInTheSlice) $ sum hits
  where
    hits = map toHit $ take dotsInTheSlice coordinateStream
    (xGen, yGen) = split gen
    coordinateStream = zip (randomRs (left, right) xGen) $ randomRs (-1, 1) yGen

-- | Total area of the unit square
totalArea :: Double
totalArea = (2 * radius) ** 2

-- | Runs MC process on a specified number of slices, throwing a specified number of dots
runCircleMC :: RandomGen g => Int -> Int -> g -> Double
runCircleMC sliceNumber dotsPerSlice gen = totalArea / (fromIntegral sliceNumber) * sum resultsOnSlices
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

-- | Runs MC process with supplied parameters measuring the time needed for that
runCircleMCBattery :: AnnotatedGenList -> Benchmark
runCircleMCBattery gens = bgroup "MC_Circle" $ do
  sliceInstance <- slices
  dotsInstance <- dots
  (name, gen) <- gens
  return $ bench (show sliceInstance ++ "_" ++ show dotsInstance ++ "_" ++ name)
    $ nf (runCircleMC sliceInstance dotsInstance) gen
