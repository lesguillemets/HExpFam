{-# LANGUAGE TypeFamilies #-}
module Data.ExponentialFamily.Class where

import Data.ExponentialFamily.Integration

type Probability = Double

class ProbDensity d where
    type Point d
    -- |
    -- f(x|θ), assuming θ is already given in d.
    -- Point d is usually Double, but sometimes it may be some other
    -- structures.
    f :: d -> Point d -> Probability

-- | Specialised, handy version of @expectVal@.
expectValD :: (Double -> Probability)         -- ^ probability density function
           -> (Double -> Double)              -- ^ Gives the value at that
           -> Double                          -- ^ Lower
           -> Double                          -- ^ Upper
           -> Double                          -- ^ dx
           -> Double
expectValD p f lower upper dx =
    sum $ map ((*dx) . (\x -> f x * p x)) [lower, lower+dx..upper]


expectVal :: (ProbDensity d, IntegrateConfig iconf, Point d ~ Domain iconf)
          => d                       -- ^ probability density function
          -> (Point d -> Double)     -- ^ Gives the value at that point
          -> iconf                   -- ^ the points and width of the integration
          -> Double                  -- ^ expectation

-- | Calculates the expected value of a function under a distribution.
expectVal dist func config =
    sum $ map ((*dx)  . (\x -> func x * p x)) xs
    where
        p = f dist
        xs = integrateValues config
        dx = integrateWidth config
