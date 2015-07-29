{-# LANGUAGE ExistentialQuantification #-}
module Test.PRNGBench.GenList where

import System.Random (RandomGen, next, split)

data SomeGen = forall g. RandomGen g => SG g

type AnnotatedGen = (String, SomeGen)

type AnnotatedGenIO = IO (String, SomeGen)

type AnnotatedGenList = [AnnotatedGen]
type AnnotatedGenListIO = IO [AnnotatedGen]

instance RandomGen SomeGen where
  next (SG gen) = let (i, newGen) = next gen in (i, SG newGen)
  split (SG gen) = let (g1, g2) = split gen in (SG g1, SG g2)
