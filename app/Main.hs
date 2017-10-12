module Main where

import Data.ExponentialFamily.Density
import Data.ExponentialFamily.Distribution.Normal
import Data.ExponentialFamily.Distribution.Binomial
import Data.ExponentialFamily.ThetaEta
import Data.Ratio

main :: IO ()
main = do
    print $ expectValD (f $ Normal 0 10) (const 1) (-1000) 1000 0.01
    print $ (fromη (Eta (0,0)) ::  Normal)
    where
        n `c` x = numerator $ product [(n-i) % (x-i) | i <- [0..x-1]]
