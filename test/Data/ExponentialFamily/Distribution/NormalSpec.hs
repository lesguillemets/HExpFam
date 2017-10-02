{-# LANGUAGE ScopedTypeVariables #-}
module Data.ExponentialFamily.Distribution.NormalSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property as QP
import Test.QuickCheck.Arbitrary

import Control.Arrow

import Data.ExponentialFamily.Distribution.Normal
import Data.ExponentialFamily.Density
import Data.ExponentialFamily.Integration
import Data.ExponentialFamily.ThetaEta


spec :: Spec
spec = do
    describe "normal distribution" $ do
        it "sums up to 1" $ property $ \(norm::Normal) ->
            (<0.001)  . abs $ 1 - expectVal (normConfig norm) norm (const 1)

        it "logF is nicely defined" $ property $ \(n::Normal) -> do
            x <- choose (-200,200)
            return . (<0.001)  . abs $ logF n x - (log $ f n x)
            
        it "fromθ . toθ is id" $ property $ \(par :: Normal) ->
            (fromθ . toθ $ par) `closeEnough` par
        it "fromη . toη is id" $ property $ \(par :: Normal) ->
            (fromη . toη $ par) `closeEnough` par
        it "modifyθ +k . -k is id" $ property $ \((par,k) :: (Normal, Double)) ->
                (modifyθ (first $ subtract k) . modifyθ (first (+k))) par `closeEnough` par
                    &&
                    (modifyθ (second $ subtract k) . modifyθ (second (+k))) par `closeEnough` par
        it "modifyη +k . -k is id" $ property $ \((par,k) :: (Normal, Double)) ->
                (modifyη (first $ subtract k) . modifyη (first (+k))) par `closeEnough` par
                    &&
                    (modifyη (second $ subtract k) . modifyη (second (+k))) par `closeEnough` par
        it "looks ok" $ property $ \(x0 :: Normal) ->
            let x1 = modifyθ (first (+0.1)) x0
                x2 = modifyη (second (+0.1)) x1
                kld1 = kld x0 x2
                kld2 = kld x0 x1 + kld x1 x2
                in  abs (kld1 - kld2) < 0.1

closeEnough :: Normal -> Normal -> Bool
closeEnough (Normal a b) (Normal c d) =
    abs (a - c) < 0.0001 && abs (b - d) < 0.0001

kld n0 n1 = kullbackLeiblerDivergence (norm2Config n0 n1) n0 n1

instance Arbitrary Normal where
    arbitrary = do
        μ <- choose (-100,100)
        σ2 <- choose (0.01, 10000)
        return $ Normal μ σ2

normConfig :: Normal -> IntegrateDouble
normConfig (Normal μ σ2) =
    let σ = sqrt σ2
        in
    IntegrateDouble (μ-4*σ) (μ+4*σ) (8*σ/10000)


norm2Config :: Normal -> Normal -> IntegrateDouble
norm2Config (Normal μ0 σ20) (Normal μ1 σ21)=
    let σ0 = sqrt σ20
        σ1 = sqrt σ21
        lower = min (μ0-2*σ0) (μ1-2*σ1)
        upper = max (μ0+2*σ0) (μ1+2*σ1)
        in
    IntegrateDouble  lower upper ((upper-lower)/10000)
