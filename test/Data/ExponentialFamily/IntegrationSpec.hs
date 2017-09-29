module Data.ExponentialFamily.IntegrationSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Arbitrary
import Data.ExponentialFamily.Integration

spec :: Spec
spec = do
    describe "integration of real functions" $ do
        it "int cos = sin" $ property $ \ (ZeroOne x') (ZeroOne y') ->
            let x = 10*pi * (subtract 0.5 $ min x' y')
                y = 10*pi * (subtract 0.5 $ max x' y')
                calced = integrate (mkconf x y) cos
                analy = sin y - sin x
                in calced `closeEnough` analy
        it "int x^2 = x^3/3" $ property $ do
            x' <- choose (-10,10)
            y' <- choose (-10,10)
            let x = min x' y'
                y = max x' y'
                calced = integrate (mkconf x y) (^2)
                analy = let f a = a^3 / 3 in f y - f x
            return $ calced `closeEnough` analy
        it "int (1/x)= log" $ property $ do
            x' <- choose (10,10000)
            y' <- choose (10,10000)
            let x = min x' y'
                y = max x' y'
                calced = integrate (mkconf x y ) (1/)
                analy = log y - log x
            return $ calced `closeEnough` analy

th :: Double
th = 0.0001
shouldBeCloseEnough :: Double -> Double -> Expectation
shouldBeCloseEnough x y = abs (x-y) `shouldSatisfy` (< th)

closeEnough :: Double -> Double -> Bool
closeEnough x y = abs (x-y) < th

mkconf :: Double -> Double -> IntegrateDouble
mkconf x y = IntegrateDouble x y ((y-x) / 50000)

newtype ZeroOne = ZeroOne Double deriving (Show)
instance Arbitrary ZeroOne where
    arbitrary = ZeroOne <$> choose (0,1)
