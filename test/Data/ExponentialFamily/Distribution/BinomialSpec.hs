module Data.ExponentialFamily.Distribution.BinomialSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property as QP
import Test.QuickCheck.Arbitrary

import Data.ExponentialFamily.Distribution.Binomial
import Data.ExponentialFamily.Integration
import Data.ExponentialFamily.Density
import Data.ExponentialFamily.ThetaEta


spec :: Spec
spec = do
    describe "binominal distribution" $ do
        it "sums up to 1" $ property $ do
            n <- choose (1,15)
            p <- choose (0,1)
            return . (<0.001)  . abs $ 1 - expectVal [0..n] (Binomial n p) (const 1)

