{-# LANGUAGE TypeFamilies #-}
module Data.ExponentialFamily.Distribution.Binomial where

import Data.ExponentialFamily.Density
import Data.ExponentialFamily.ThetaEta

import Numeric.SpecFunctions (choose)

-- | Binomial distribution, where you conduct n trials with probability p.
data Binomial = Binomial { _n :: Int
                         , _p :: Double
                         } deriving (Show, Eq)

instance ProbDensity Binomial where
    -- | The probability of succeeding @success@ ∈ ℕ
    type Point Binomial = Int
    f (Binomial n p) success =
        (n `choose` success) * p^success * (1-p)^(n-success)
