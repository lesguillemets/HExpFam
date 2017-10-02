{-# LANGUAGE TypeFamilies #-}
module Data.ExponentialFamily.Distribution.Uniform where

import Data.ExponentialFamily.Density
import Data.ExponentialFamily.ThetaEta


-- | Uniform distribution. Range is inclusive.
-- Make a distribution either by i)  explicitly giving the range and p,
-- or @fromRange@ function.
-- >>> unif0 = Uniform (1,10) 0.1 :: Uniform Int
-- >>> f unif0 5
-- 0.1
-- >>> f unif0 100
-- 0
-- >>> fromRange (1,10) == unif0
-- True
-- >>> unif1 = Uniform (1,10) 0.1 :: Uniform Double
-- >>> f unif1 5.1
-- 0.1
-- >>> f unif1 100
-- 0
-- >>> fromRange (1,10) == unif1
-- True
data Uniform a = Uniform { _range :: (a,a)
                         , _p     :: Double
                         } deriving (Show, Eq)

-- | Helper class for auto-generating @_p@ of @Uniform a@.
-- TODO : Fix the name
class UnifFromRange a where
    fromRange :: (a,a) -> Uniform a

instance UnifFromRange Double where
    fromRange r@(l,h) = Uniform r (1/(h-l))

instance UnifFromRange Int where
    fromRange r@(l,h) = Uniform r (1/fromIntegral (1+h-l))
instance UnifFromRange Integer where
    fromRange r@(l,h) = Uniform r (1/fromIntegral (1+h-l))

instance Ord a => ProbDensity (Uniform a) where
    type Point (Uniform a) = a
    f (Uniform (l,h) p) x = if l <= x && x <= h
                               then p
                               else 0
    logF (Uniform (l,h) p) x = if l <= x && x <= h
                                  then log p
                                  else 0
