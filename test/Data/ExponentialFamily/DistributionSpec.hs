module Data.ExponentialFamily.DistributionSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Arbitrary

import Data.ExponentialFamily.Integration
import Data.ExponentialFamily.Distribution
import Data.ExponentialFamily.Density
import Data.ExponentialFamily.ThetaEta

spec :: Spec
spec = do
    describe "normal distribution" $ do
        it "sums up to 1" $ property $ do
            μ <- choose (-10, 10)
            σ2 <- choose (0, 100)
            return . (<0.001)  . abs $ 1 - expectVal (IntegrateDouble (-100) 100 0.01) (Normal μ σ2) (const 1)
        it "fromθ . toθ is id" $ property $ do
            par <- Normal <$> choose (-10, 10) <*> choose (-10, 10)
            return $ (fromθ . toθ $ par) `closeEnough` par
        it "fromη . toη is id" $ property $ do
            par <- Normal <$> choose (-10, 10) <*> choose (-10, 10)
            return $ (fromη . toη $ par) `closeEnough` par

    describe "binominal distribution" $ do
        it "sums up to 1" $ property $ do
            n <- choose (1,15)
            p <- choose (0,1)
            return . (<0.001)  . abs $ 1 - expectVal [0..n] (Binomial n p) (const 1)

closeEnough :: Normal -> Normal -> Bool
closeEnough (Normal a b) (Normal c d) =
    abs (a - c) < 0.0001 && abs (b - d) < 0.0001
