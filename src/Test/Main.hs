module Main(main) where

import Control.Monad (join)

import System.Random (RandomGen)

import System.Random.TF.Init (newTFGen)
import System.Random.PCG.Fast.Pure (save, create)
import System.Random.SplitMix (newSplitMix64)

import Test.PRNGBench (genToAnnotatedGen, runGroups)
import Test.PRNGBench.GenList (AnnotatedGenIO, AnnotatedGenListIO)

-- | Given a name for a generator, turns an action for getting a said random
-- generator into an action for getting a boxed and named one
genToAnnotatedGenIO :: RandomGen g => String -> IO g -> AnnotatedGenIO
genToAnnotatedGenIO n gen = gen >>= return . genToAnnotatedGen n

-- | Action to get all default generators to test against
annotatedGensIO :: AnnotatedGenListIO
annotatedGensIO = sequence [genToAnnotatedGenIO "SplitMix" newSplitMix64,
                            genToAnnotatedGenIO "TFRandom" newTFGen,
                            genToAnnotatedGenIO "PCGRandom" $ join $ fmap save create]

-- | Runs the default groups on the default generators
main :: IO ()
main = annotatedGensIO >>= runGroups
