{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.ExponentialFamily where

import Data.ExponentialFamily.ThetaEta
import Data.ExponentialFamily.Density

-- | Class of Distributions that belongs to ExponentialFamily.
-- Follows this notation:
-- p(x;θ) = exp( <t(x), θ> - F(θ) + k(x) ).
class (ThetaEta d, ProbDensity d, InnerProd (TEParam d)) => ExponentialFamily d where
    -- | t(x). Perhaps make it Point d -> TEParam d,
    -- but it's less convenient for typecheck
    _t :: d -> Point d -> TEParam d
    
    -- | F(θ). Sometimes called A(η).
    _Fθ :: d -> Double
    
    -- | k (x). Sometimes log h(x).
    _k :: d -> Point d -> Double
    
    -- | log( p(x;θ) ).
    logPOf :: d -> Point d -> Double
    logPOf dist x =
        (_t dist x) `innerProd` (unθ $ toθ dist) - _Fθ dist + _k dist x
    
    pOf :: d -> Point d -> Double
    pOf dist x = exp $ logPOf dist x

class InnerProd a where
    innerProd :: a -> a -> Double

instance InnerProd (Double, Double) where
    innerProd (x,y) (z,w) = x*z + y*w
