module Main where

import Naiive
import Data.ExponentialFamily.Density
import Data.ExponentialFamily.Distribution.Normal
import Data.ExponentialFamily.Distribution.Binomial
import Data.Ratio

main :: IO ()
main = do
    print $ calc  (binom 10) (0.2) 3
    print $ (0.2^3 * 0.8^7) * (fromIntegral $ 10 `c` 3)
    print $ expectValD (f $ Normal 0 10) (const 1) (-1000) 1000 0.01
    where
        n `c` x = numerator $ product [(n-i) % (x-i) | i <- [0..x-1]]
