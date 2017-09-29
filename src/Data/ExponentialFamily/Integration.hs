{-# LANGUAGE TypeFamilies #-}
module Data.ExponentialFamily.Integration where

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


-- | Integrate a function.
-- non-Double valued functions are currently not supported.
integrate ::
       IntegrateConfig i
    => i -- ^ How?
    -> (Domain i -> Double) -- ^ function to integrate.
    -> Double
integrate i f = sum . map ((* integrateWidth i) . f) $ integrateValues i

--
-- | Integrate from @_lower@ to @_upper@, using boxes whose width is @_dx@.
--
-- >>> take 5 . show $ integrate (IntegrateDouble 0 (pi/2) 0.001) $ sin
-- "1.000"
data IntegrateDouble = IntegrateDouble { _lower :: Double
                                       , _upper :: Double
                                       , _dx :: Double}
instance IntegrateConfig IntegrateDouble where
    type Domain IntegrateDouble = Double
    integrateValues (IntegrateDouble l u dx) = [l+dx/2, l+3*dx/2..u-dx/2]
    integrateWidth = _dx


-- | For Discrete distributions, use all the values with equal weight, 1.
--
-- >>> data Foo = Foo | Bar | Baz | Foz deriving (Enum, Bounded)
-- >>> integrate (IntegrateDiscrete :: IntegrateDiscrete Foo) (fromIntegral . fromEnum)
-- 6.0
data IntegrateDiscrete a = IntegrateDiscrete
instance (Enum a, Bounded a) => IntegrateConfig (IntegrateDiscrete a) where
    type Domain (IntegrateDiscrete a) = a
    integrateValues _ = enumFrom minBound
    integrateWidth = const 1

-- | Use all the values given with equal weight of 1.
--
-- >>> integrate  [0..9] (fromIntegral . succ)
-- 55.0
instance IntegrateConfig [a] where
    type Domain [a] = a
    integrateValues = id
    integrateWidth = const 1
