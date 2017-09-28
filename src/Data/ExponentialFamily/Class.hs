{-# LANGUAGE TypeFamilies #-}
module Data.ExponentialFamily.Class where

type Probability = Double

class ProbDensity d where
    type Point d
    -- |
    -- f(x|θ), assuming θ is already given in d.
    -- Point d is usually Double, but sometimes it may be some other
    -- structures.
    f :: d -> Point d -> Probability

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

-- | Information about how to calculate the integral.
--   @Domain i@ is usually Double, but we can define integration of some
--   function f : ℂ → ℝ on a line, in which case @Domain i@ might be
--   @Complex@. We would like to use this to calculate the expected value
--   over discrete random distribution too, for which @Domain i@ shall be
--   an iteration over the support.
class IntegrateConfig i where
    -- | We will calculate integral of f : @(Domain i) -> Double@.
    type Domain i
    -- | See the implementation of @IntegrateDouble@ to get grasp of this.
    integrateValues :: i -> [Domain i]
    integrateWidth :: i -> Double


-- | Integrate from @_lower@ to @_upper@, using boxes whose width is @_dx@.
--
data IntegrateDouble = IntegrateDouble { _lower :: Double
                                       , _upper :: Double
                                       , _dx :: Double}
instance IntegrateConfig IntegrateDouble where
    type Domain IntegrateDouble = Double
    integrateValues (IntegrateDouble l u dx) = [l, l+dx..u]
    integrateWidth = _dx

-- | For Discrete distributions, use all the value with equal weight, 1.
data IntegrateDiscrete a = IntegrateDiscrete
instance (Enum a, Bounded a) => IntegrateConfig (IntegrateDiscrete a) where
    type Domain (IntegrateDiscrete a) = a
    integrateValues _ = enumFrom minBound
    integrateWidth = const 1
