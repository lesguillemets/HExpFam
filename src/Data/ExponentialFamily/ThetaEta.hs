{-# LANGUAGE TypeFamilies #-}
module Data.ExponentialFamily.ThetaEta where


type family TEParam dist :: *

class ThetaEta a where
    toθ :: a -> Theta a
    fromθ :: Theta a -> a
    toη :: a -> Eta a
    fromη :: Eta a -> a
    modifyθ :: (TEParam a -> TEParam a) -> a -> a
    modifyθ f = fromθ . θmap f . toθ
    modifyη :: (TEParam a -> TEParam a) -> a -> a
    modifyη f = fromη . ηmap f . toη


newtype Theta a = Theta (TEParam a)
θmap :: (TEParam a -> TEParam b) -> Theta a -> Theta b
θmap f (Theta a) = Theta (f a)
newtype Eta a = Eta (TEParam a)
ηmap :: (TEParam a -> TEParam b) -> Eta a -> Eta b
ηmap f (Eta a) = Eta (f a)
