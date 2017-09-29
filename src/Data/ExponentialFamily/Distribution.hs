{-# LANGUAGE TypeFamilies #-}
module Data.ExponentialFamily.Distribution where

import Data.ExponentialFamily.Density

import Numeric.SpecFunctions (choose)

data Normal = Normal { _μ :: Double
                     , _σ :: Double
                     } deriving (Show)

instance ProbDensity Normal where
    type Point Normal = Double
    f (Normal μ σ) x = exp ( -(x-μ)^2 / (2*σ^2) ) / sqrt (2*pi*σ^2)

data Binomial = Binomial { _n :: Int
                         , _p :: Double
                         } deriving (Show)

instance ProbDensity Binomial where
    type Point Binomial = Int
    f (Binomial n p) success =
        (n `choose` success) * p^success * (1-p)^(n-success)
