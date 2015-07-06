module Main where

import System.Random (RandomGen)

import System.Random.TF.Init
import System.Random.PCG.Fast.Pure

import SplitMix

import PRNGBench
import PRNGBench.GenList (AnnotatedGenIO, AnnotatedGenListIO)

genToAnnotatedGenIO :: RandomGen g => String -> IO g -> AnnotatedGenIO
genToAnnotatedGenIO n gen = gen >>= return . genToAnnotatedGen n

join :: Monad m => m (m a) -> m a
join v = v >>= id

annotatedGensIO :: AnnotatedGenListIO
annotatedGensIO = sequence [genToAnnotatedGenIO "SplitMix" newSplitMix64,
                            genToAnnotatedGenIO "TFRandom" newTFGen,
                            genToAnnotatedGenIO "PCGRandom" $ join $ fmap save create]

main :: IO ()
main = annotatedGensIO >>= runGroups
