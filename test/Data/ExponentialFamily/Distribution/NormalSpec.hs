{-# LANGUAGE ScopedTypeVariables #-}
module Data.ExponentialFamily.Distribution.NormalSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property as QP
import Test.QuickCheck.Arbitrary
import Data.ExponentialFamily.Distribution.Helper

import Control.Arrow

import Data.ExponentialFamily
import Data.ExponentialFamily.Distribution.Normal
import Data.ExponentialFamily.Density
import Data.ExponentialFamily.Integration
import Data.ExponentialFamily.ThetaEta


spec :: Spec
spec = do
    describe "basic properties" $ do
        it "sums up to 1" $ property $ sumsTo1' normConfig

        it "logF is nicely defined" $ property $ \(n::Normal) -> do
            x <- choose (-200,200)
            return . (<0.001)  . abs $ logF n x - (log $ f n x)

    describe "eta-theta" $ do
        it "fromθ . toθ is id" $ property $ fromθtoθIsID closeEnough

        it "fromη . toη is id" $ property $ fromηtoηIsID closeEnough
        it "modifyθ +k . -k is id" $ property $ \((par,k) :: (Normal, Double)) ->
                modifyθPreservesID (first $ subtract k) (first $ (+k)) closeEnough par
                &&
                modifyθPreservesID (second $ subtract k) (second $ (+k)) closeEnough par
        it "modifyη +k . -k is id" $ property $ \((par,k) :: (Normal, Double)) ->
                modifyηPreservesID (first $ subtract k) (first $ (+k)) closeEnough par
                &&
                modifyηPreservesID (second $ subtract k) (second $ (+k)) closeEnough par
        it "looks ok" $ property $ \(x0 :: Normal, Small diff0, Small diff1) ->
            let x1 = modifyθ (first (+ (0.001*fromInteger diff0))) x0
                x2 = modifyη (second (+(0.001*fromIntegral diff1))) x1
                kld1 = kld x0 x2
                kld2 = kld x0 x1 + kld x1 x2
                in  abs (kld1 - kld2) < 0.001

    describe "exponential family" $ do
        it "logF is close enough, when calculated by its ExponentialFamily" $ property $ (\(n::Normal,x) -> (<0.001) . abs $ logF n x - logPOf n x)
            

closeEnough :: Normal -> Normal -> Bool
closeEnough (Normal a b) (Normal c d) =
    abs (a - c) < 0.001 && abs (b - d) < 0.001

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
        lower = min (μ0-8*σ0) (μ1-8*σ1)
        upper = max (μ0+8*σ0) (μ1+8*σ1)
        in
    IntegrateDouble  lower upper ((upper-lower)/40000)
