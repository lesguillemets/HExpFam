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
    logF :: d -> Point d -> Double
    logF p x = log $ f p x

-- | Specialised, handy version of @expectVal@.
expectValD ::
       (Double -> Probability) -- ^ probability density function
    -> (Double -> Double) -- ^ Gives the value at that
    -> Double -- ^ Lower
    -> Double -- ^ Upper
    -> Double -- ^ dx
    -> Double
expectValD p f lower upper dx =
    sum $ map ((* dx) . (\x -> f x * p x)) [lower,lower + dx .. upper]

expectVal ::
       (ProbDensity d, IntegrateConfig iconf, Point d ~ Domain iconf)
    => iconf -- ^ the points and width of the integration
    -> d -- ^ probability density function
    -> (Point d -> Double) -- ^ Gives the value at that point
    -> Double -- ^ expectation
-- | Calculates the expected value of a function under a distribution.
expectVal config dist func = sum $ map ((* dx) . (\x -> func x * p x)) xs
  where
    p = f dist
    xs = integrateValues config
    dx = integrateWidth config

kullbackLeiblerDivergence ::
       ( ProbDensity d0
       , ProbDensity d1
       , IntegrateConfig iconf
       , Point d0 ~ Point d1
       , Point d0 ~ Domain iconf
       )
    => iconf
    -> d0
    -> d1
    -> Double
kullbackLeiblerDivergence c d0 d1 =
    expectVal c d0 (\x -> logF d0 x / logF d1 x)
