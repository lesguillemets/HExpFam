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

import Data.ExponentialFamily.Distribution.Helper


spec :: Spec
spec = do
    describe "binominal distribution" $
        it "sums up to 1" $ property $ sumsTo1' binomConf
    describe "theta-eta" $ do
        it "fromθ . toθ is id" $ property $ fromθtoθIsID closeEnough
        it "fromη . toη is id" $ property $ fromηtoηIsID closeEnough
        it "modifyθ +k . -k is id" $ property $ \((bin,Dk k) :: (Binomial, Dk)) ->
                (modifyθ (first $ subtract k) . modifyθ (first (+k))) bin `closeEnough` bin

closeEnough :: Binomial -> Binomial -> Bool
closeEnough (Binomial a b) (Binomial c d) =
    a == c && abs (b - d) < 0.0001

instance Arbitrary Binomial where
    arbitrary = Binomial <$> choose (1,15) <*> choose (0,1)

binomConf :: Binomial -> [Int]
binomConf (Binomial n _) = [0..n]

newtype Dk = Dk Double
instance Show Dk where
    show (Dk k) = show k
instance Arbitrary Dk where
    arbitrary = Dk <$> choose (-0.02,0.02)
