{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.ExponentialFamily where

import Data.ExponentialFamily.ThetaEta
import Data.ExponentialFamily.Density
import Data.ExponentialFamily.KullbackLeibler

-- | Class of Distributions that belongs to ExponentialFamily.
-- Follows this notation:
-- p(x;θ) = exp( <t(x), θ> - F(θ) + k(x) ).
class (ThetaEta d, ProbDensity d, VLike (TEParam d)) => ExponentialFamily d where
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

instance ExponentialFamily d => KullbackLeibler d d where
    dKL p0 p1 =
       let θ0 = unθ $ toθ p0
           θ1 = unθ $ toθ p1
           η0 = unη $ toη p0
           in negate (_Fθ p0) + _Fθ p1 + (θ0 <-> θ1) `innerProd` η0

-- | TODO : too dirty
class VLike a where
    (<+>) :: a -> a -> a
    (<->) :: a -> a -> a
    innerProd :: a -> a -> Double

instance VLike (Double, Double) where
    (x,y) <+> (z,w) = (x+z, y+w)
    (x,y) <-> (z,w) = (x-z, y-w)
    innerProd (x,y) (z,w) = x*z + y*w
