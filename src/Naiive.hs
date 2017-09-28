{-# LANGUAGE RecordWildCards #-}
module Naiive  where
import Data.Ratio

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Prob = Double

data ExpFam param a
    = ExpF { _h :: a -> Double
           , _c :: param -> Double
           , _w :: [param -> Double]
           , _t :: [a -> Double] -- TODO : Assert length _w == length _t
           }

-- | Given a family, actual parameter and a x, get the probability there
calc :: ExpFam param a -> param -> a -> Double
calc ExpF {..} θ x = _h x *  _c θ * exp pp
    where
        (<$$>) fs val = map (\f -> f val) fs
        pp =  sum $ zipWith (*)  (_w <$$> θ) (_t <$$> x)


binom :: Int -> ExpFam Double Int
binom n = ExpF (fromIntegral . (n `c`))
               (\p -> (1-p)^n)
               [\p -> log (p / (1-p))]
               [fromIntegral]
    where
        n `c` x = numerator $ product [(n-i) % (x-i) | i <- [0..x-1]]

