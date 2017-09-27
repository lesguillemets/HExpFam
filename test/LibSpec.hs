module LibSpec (spec) where

import Test.Hspec
import Lib
import Data.Ratio (numerator, (%))


spec :: Spec
spec = do
    describe "calc binom returns the same value as done with bare hands" $ do
        it "n=10, p=0.3, occurs 8 times" $ do
            cmpWithBare 10 0.3 8 `shouldSatisfy` <
            abs (  calc  (binom 10) (0.3) 8
                - (0.3^8 * 0.7^2) * (fromIntegral $ 10 `c` 8)
                )`shouldSatisfy` (<0.0000001)
    where
        n `c` x = numerator $ product [(n-i) % (x-i) | i <- [0..x-1]]
