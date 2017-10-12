{-# LANGUAGE MultiParamTypeClasses #-}
module Data.ExponentialFamily.KullbackLeibler where

import Data.ExponentialFamily.Density

class (ProbDensity d0, ProbDensity d1) => KullbackLeibler d0 d1 where
    -- | Kullback-Leibler divergence.
    dKL :: d0 -> d1 -> Double
