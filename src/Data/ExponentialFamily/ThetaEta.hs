{-# LANGUAGE TypeFamilies #-}
module Data.ExponentialFamily.ThetaEta where

class ThetaEta a where
    data Param a
    fromParam :: Param a -> a
    toParam :: a -> Param a
    toθ :: Param a -> Theta
    fromθ :: Theta -> Param a
    toη :: Param a -> Eta
    fromη :: Eta -> Param a
    modifyθ :: (Theta -> Theta) -> Param a -> Param a
    modifyθ f = fromθ . f . toθ
    modifyη :: (Eta -> Eta) -> Param a -> Param a
    modifyη f = fromη . f . toη

type Eta = [Double]
type Theta = [Double]
