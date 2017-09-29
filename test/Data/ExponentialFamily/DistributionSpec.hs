module Data.ExponentialFamily.DistributionSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Arbitrary

import Data.ExponentialFamily.Integration
import Data.ExponentialFamily.Distribution
import Data.ExponentialFamily.Density

spec :: Spec
spec = do
    describe "normal distribution" $ do
        it "sums up to 1" $ property $ do
            μ <- choose (-10, 10)
            σ <- choose (-10, 10)
            return . (<0.001)  . abs $ 1 - expectVal (IntegrateDouble (-100) 100 0.01) (Normal μ σ) (const 1)
    describe "binominal distribution" $ do
        it "sums up to 1" $ property $ do
            n <- choose (1,15)
            p <- choose (0,1)
            return . (<0.001)  . abs $ 1 - expectVal [0..n] (Binomial n p) (const 1)
