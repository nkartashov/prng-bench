{-# LANGUAGE ExistentialQuantification #-}
module Test.PRNGBench.GenList
  ( SomeGen(..)
  , AnnotatedGen
  , AnnotatedGenIO
  , AnnotatedGenList
  , AnnotatedGenListIO
  ) where

import Control.Arrow ((***), second)

import System.Random (RandomGen, next, split)

-- | Wraps any random number generator into a box
data SomeGen = forall g. RandomGen g => SG g

type AnnotatedGen = (String, SomeGen)
type AnnotatedGenIO = IO (String, SomeGen)
type AnnotatedGenList = [AnnotatedGen]
type AnnotatedGenListIO = IO [AnnotatedGen]

instance RandomGen SomeGen where
  next (SG gen) = second SG $ next gen
  split (SG gen) = SG *** SG $ split gen
