module Main where

import System.Random (RandomGen)

import Criterion.Main

import System.Random.TF.Init

import SplitMix

import PRNGBench

annotatedGenToIOBenchmark :: RandomGen g => String -> IO g -> IO Benchmark
annotatedGenToIOBenchmark n gen = gen >>= \g -> return $ genToBenchGroup n g

benchsIO :: IO [Benchmark]
benchsIO = sequence [annotatedGenToIOBenchmark "SplitMix" newSplitMix64, annotatedGenToIOBenchmark "TFRandom" newTFGen]

main :: IO ()
main = benchsIO >>= defaultMain
