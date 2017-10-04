{-# LANGUAGE TypeFamilies #-}
module Data.ExponentialFamily.Distribution.Helper where

import Test.QuickCheck
import Test.QuickCheck.Property as QP
import Test.QuickCheck.Arbitrary

import Data.ExponentialFamily.Density
import Data.ExponentialFamily.Integration
import Data.ExponentialFamily.ThetaEta


sumsTo1 ::
       (ProbDensity d, IntegrateConfig i, Domain i ~ Point d)
    => (d -> i)
    -> Double
    -> d
    -> Bool
sumsTo1 configMaker threshold d = abs (1-sums) < threshold
    where
        sums = expectVal (configMaker d) d (const 1)

sumsTo1' ::
       (ProbDensity d, IntegrateConfig i, Domain i ~ Point d)
    => (d -> i)
    -> d
    -> Bool
sumsTo1' c d = sumsTo1 c thresholdm d



thresholdm :: Double
thresholdm = 0.0001

fromθtoθIsID :: ThetaEta d => (d -> d -> Bool) -> d -> Bool
fromθtoθIsID closeEnough d = (fromθ . toθ $ d) `closeEnough` d

fromηtoηIsID :: ThetaEta d => (d -> d -> Bool) -> d -> Bool
fromηtoηIsID closeEnough d = (fromη . toη $ d) `closeEnough` d

modifyθPreservesID ::
       ThetaEta d
    => (TEParam d -> TEParam d)
    -> (TEParam d -> TEParam d)
    -> (d -> d -> Bool)
    -> d
    -> Bool
modifyθPreservesID f invF closeEnough d =
    (modifyθ invF . modifyθ f) d `closeEnough` d
        &&
    (modifyθ f . modifyθ invF) d `closeEnough` d

modifyηPreservesID ::
       ThetaEta d
    => (TEParam d -> TEParam d)
    -> (TEParam d -> TEParam d)
    -> (d -> d -> Bool)
    -> d
    -> Bool
modifyηPreservesID f invF closeEnough d =
    (modifyη invF . modifyη f) d `closeEnough` d
        &&
    (modifyη f . modifyη invF) d `closeEnough` d

