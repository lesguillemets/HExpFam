{-# LANGUAGE TypeFamilies #-}
module Data.ExponentialFamily.Class where


type Probability = Double

class ProbDensity d where
    type Point d
    -- |
    -- f(x|θ), assuming θ is already given in a.
    -- Point a is usually Double, but sometimes it may be some other
    -- structures.
    f :: d -> Point d -> Probability

expectVal :: (Double -> Probability)         -- probability density function
          -> (Double -> Double)              -- Gives the value at that
          -> Double                          -- Lower
          -> Double                          -- Upper
          -> Double                          -- dx
          -> Double
expectVal p f lower upper dx =
    sum $ map ((*dx) . (\x -> f x * p x)) [lower, lower+dx..upper]

