module Data.ExponentialFamily.DistributionSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property as QP
import Test.QuickCheck.Arbitrary

import Control.Arrow

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
        it "logF is nicely defined" $ property $ do
            n <- Normal <$> choose (-100, 100) <*>  choose (0.01, 10000)
            x <- choose (-200,200)
            return . (<0.001)  . abs $ logF n x - (log $ f n x)
            
        it "fromθ . toθ is id" $ property $ do
            par <- Normal <$> choose (-10, 10) <*> choose (0, 100)
            return $ (fromθ . toθ $ par) `closeEnough` par
        it "fromη . toη is id" $ property $ do
            par <- Normal <$> choose (-10, 10) <*> choose (0, 100)
            return $ (fromη . toη $ par) `closeEnough` par
        it "modifyθ +k . -k is id" $ property $ do
            par <- Normal <$> choose (-10, 10) <*> choose (0, 100)
            k <- arbitrary
            return $ (modifyθ (first $ subtract k) . modifyθ (first (+k))) par `closeEnough` par
                    &&
                    (modifyθ (second $ subtract k) . modifyθ (second (+k))) par `closeEnough` par
        it "modifyη +k . -k is id" $ property $ do
            par <- Normal <$> choose (-10, 10) <*> choose (0, 100)
            k <- arbitrary
            return $  (modifyη (first $ subtract k) . modifyη (first (+k))) par `closeEnough` par
                    &&
                    (modifyη (second $ subtract k) . modifyη (second (+k))) par `closeEnough` par
        it "looks ok" $ property $ do
            x0 <- Normal <$> choose (-10, 10) <*> choose (0.1, 100)
            let x1 = modifyθ (first (+0.1)) x0
                x2 = modifyη (second (+0.1)) x1
                kld1 = kld x0 x2
                kld2 = kld x0 x1 + kld x1 x2
            return $ if abs (kld1 - kld2) < 0.1
                        then succeeded
                        else failed {
                            QP.reason =
                                (show (x0, x1, x2, kld1, kld2, abs (kld1-kld2)))
                        }

    describe "binominal distribution" $ do
        it "sums up to 1" $ property $ do
            n <- choose (1,15)
            p <- choose (0,1)
            return . (<0.001)  . abs $ 1 - expectVal [0..n] (Binomial n p) (const 1)

closeEnough :: Normal -> Normal -> Bool
closeEnough (Normal a b) (Normal c d) =
    abs (a - c) < 0.0001 && abs (b - d) < 0.0001

kld = kullbackLeiblerDivergence (IntegrateDouble (-100) 100 0.01)
