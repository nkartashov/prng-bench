module Main where

import Criterion.Main

import System.Random.TF.Init
import SplitMix

import PRNGBench

annotatedGenToIOBenchmark n gen = gen >>= \g -> return $ genToBenchGroup n g

benchsIO = sequence [annotatedGenToIOBenchmark "SplitMix" newSplitMix64, annotatedGenToIOBenchmark "TFRandom" newTFGen]

main :: IO ()
main = benchsIO >>= defaultMain
