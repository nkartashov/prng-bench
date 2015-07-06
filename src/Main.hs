module Main where

import System.Random (RandomGen)

import Criterion.Main

import System.Random.TF.Init
import System.Random.PCG.Fast.Pure

import SplitMix

import PRNGBench

annotatedGenToIOBenchmark :: RandomGen g => String -> IO g -> IO Benchmark
annotatedGenToIOBenchmark n gen = gen >>= \g -> return $ genToBenchGroup n g

join :: Monad m => m (m a) -> m a
join v = v >>= id

benchsIO :: IO [Benchmark]
benchsIO = sequence [annotatedGenToIOBenchmark "SplitMix" newSplitMix64,
                     annotatedGenToIOBenchmark "TFRandom" newTFGen,
                     annotatedGenToIOBenchmark "PCGRandom" $ join $ fmap save create]

main :: IO ()
main = benchsIO >>= defaultMain
