module Data.ExponentialFamily.Distribution.UniformSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property as QP
import Test.QuickCheck.Arbitrary

import Data.ExponentialFamily.Distribution.Uniform
import Data.ExponentialFamily.Integration
import Data.ExponentialFamily.Density
import Data.ExponentialFamily.ThetaEta

spec :: Spec
spec = do
    describe "uniform distribution" $ do
        it "Uniform Int sums up to 1" $ property $ \(AR r@(x,y)) ->
            (<0.001)  . abs $ 1 - expectVal [x..y] (fromRange r :: Uniform Int) (const 1)
        it "Uniform Double sums up to 1" $ property $ \(AR r@(x,y)) ->
            x /= y ==>
            (<0.001)  . abs $ 1 - expectVal (IntegrateDouble (x-5) (y+5) ((y-x)/10000)) (fromRange r) (const 1)


newtype AR a = AR (a,a)
instance (Show a) => Show (AR a) where
    show (AR r) = show r
instance (Ord a, Arbitrary a) => Arbitrary (AR a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ AR (min x y, max x y)
