{-# LANGUAGE ScopedTypeVariables #-}
module Data.ExponentialFamily.Distribution.BinomialSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property as QP
import Test.QuickCheck.Arbitrary

import Control.Arrow
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
        it "fromθ . toθ is id" $ property $ \(bin :: Binomial) ->
            (fromθ . toθ $ bin) `closeEnough` bin
        it "fromη . toη is id" $ property $ \(bin :: Binomial) ->
            (fromη . toη $ bin) `closeEnough` bin
        it "modifyθ +k . -k is id" $ property $ \((bin,Dk k) :: (Binomial, Dk)) ->
                (modifyθ (first $ subtract k) . modifyθ (first (+k))) bin `closeEnough` bin

closeEnough :: Binomial -> Binomial -> Bool
closeEnough (Binomial a b) (Binomial c d) =
    a == c && abs (b - d) < 0.0001

instance Arbitrary Binomial where
    arbitrary = Binomial <$> choose (1,15) <*> choose (0,1)


newtype Dk = Dk Double
instance Show Dk where
    show (Dk k) = show k
instance Arbitrary Dk where
    arbitrary = Dk <$> choose (-0.02,0.02)
