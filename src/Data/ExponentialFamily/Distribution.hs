{-# LANGUAGE TypeFamilies #-}
module Data.ExponentialFamily.Distribution where

import Data.ExponentialFamily.Density
import Data.ExponentialFamily.ThetaEta

import Numeric.SpecFunctions (choose)

data Normal = Normal { _μ :: Double
                     , _σ :: Double
                     } deriving (Show, Eq)

instance ProbDensity Normal where
    type Point Normal = Double
    f (Normal μ σ) x = exp ( -(x-μ)^2 / (2*σ^2) ) / sqrt (2*pi*σ^2)

instance ThetaEta Normal where
    -- | μ and σ^2. Apparently that makes more sense
    newtype Param Normal = NormP (Double, Double) deriving (Eq)
    fromParam (NormP (μ, σ2)) = Normal μ (sqrt σ2)
    toParam (Normal μ σ) = NormP (μ, σ^2)
    toθ (NormP (μ, σ2)) = [μ/σ2, -1 / (2*σ2)]
    fromθ [θ0, θ1]      = NormP (- θ0/(2*θ1), -1/(2*θ1))
    toη (NormP (μ, σ2)) = [μ, μ^2+σ2]
    fromη [η0, η1]      = NormP (η0,  η1 - η0^2)

data Binomial = Binomial { _n :: Int
                         , _p :: Double
                         } deriving (Show, Eq)

instance ProbDensity Binomial where
    type Point Binomial = Int
    f (Binomial n p) success =
        (n `choose` success) * p^success * (1-p)^(n-success)
