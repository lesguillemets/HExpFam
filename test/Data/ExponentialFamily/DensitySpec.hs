module Data.ExponentialFamily.DensitySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Arbitrary

import Data.ExponentialFamily.Density
import Data.ExponentialFamily.Distribution.Normal
import Data.ExponentialFamily.Distribution.Uniform
import Data.ExponentialFamily.Integration

spec :: Spec
spec = do
    describe "expectVal" $ do
        it "six-sided dice" $
            expectVal [1..6] (fromRange (1,6) :: Uniform Int) fromIntegral
                `shouldBeCloseEnough` 3.5
        it "n-sided dice" $ property $ \(Positive n) ->
            expectVal [1..n] (fromRange (1,n) :: Uniform Int) fromIntegral
                `closeEnough` (fromIntegral (n+1) / 2)
        it "normal distribution : its mean" $ property $ \n ->
            let μ = _μ n
                σ = sqrt $ _σ2 n
                in expectVal (mkconf (μ-8*σ) (μ+8*σ)) n id `closeEnough` μ
    describe "Kullback-Leibler divergence" $ do
        it "N(0.4,0.4) and N(0.6,0.8) is 0.3439" $
            kullbackLeiblerDivergence c n0 n1 `shouldBeCloseEnough` 0.343972
        it "and reverse 0.9318528" $
            kullbackLeiblerDivergence c n1 n0 `shouldBeCloseEnough` 0.9318528
        where
            n0 = Normal 0.4 (0.4^2)
            n1 = Normal 0.6 (0.8^2)
            c = IntegrateDouble (- 1000) 1000 0.01

instance Arbitrary Normal where
    arbitrary = Normal <$> choose (-100,100) <*> choose (0.01, 10000)

th :: Double
th = 0.001
shouldBeCloseEnough :: Double -> Double -> Expectation
shouldBeCloseEnough x y = abs (x-y) `shouldSatisfy` (< th)

closeEnough :: Double -> Double -> Bool
closeEnough x y = abs (x-y) < th

mkconf :: Double -> Double -> IntegrateDouble
mkconf x y = IntegrateDouble x y ((y-x) / 20000)
