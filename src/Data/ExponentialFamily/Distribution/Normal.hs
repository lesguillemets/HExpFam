{-# LANGUAGE TypeFamilies #-}
module Data.ExponentialFamily.Distribution.Normal where

import Data.ExponentialFamily.Density
import Data.ExponentialFamily.ThetaEta

-- | Normal distribution, with mean μ and variance σ2.
data Normal = Normal { _μ :: Double
                     , _σ2 :: Double
                     } deriving (Show, Eq)

instance ProbDensity Normal where
    type Point Normal = Double
    f (Normal μ σ2) x = exp ( -(x-μ)^2 / (2*σ2) ) / sqrt (2*pi*σ2)
    logF (Normal μ σ2) x = -(x-μ)^2 / (2*σ2) - log(2*pi*σ2)/2

instance ThetaEta Normal where
    type TEParam Normal = (Double, Double)
    toθ (Normal μ σ2)      = Theta (μ/σ2, -1 / (2*σ2))
    fromθ (Theta (θ0, θ1)) = Normal (- θ0/(2*θ1)) (-1/(2*θ1))
    toη (Normal μ σ2)      = Eta (μ, μ^2+σ2)
    fromη (Eta (η0, η1))   = Normal η0 (η1 - η0^2)

