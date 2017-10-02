module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "src/Data/ExponentialFamily/Integration.hs"
               , "src/Data/ExponentialFamily/Distribution/Uniform.hs"
               ]

