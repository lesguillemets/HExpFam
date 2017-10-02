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

-- | (η,N) and (θ,N) for 'known' number of trials : N.
type instance TEParam Binomial = (Double, Int)
instance ThetaEta Binomial where
    toθ (Binomial n p)  = Theta (log p / (1-p), n)
    fromθ (Theta (logit, n))
        = Binomial n (exp logit / (1+exp logit))
    toη (Binomial n p)  = Eta (fromIntegral n*p, n)
    fromη (Eta (np, n)) = Binomial n (np/fromIntegral n)
