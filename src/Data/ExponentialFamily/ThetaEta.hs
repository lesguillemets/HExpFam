{-|
Anything related to θ-η-coordinates.
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.ExponentialFamily.ThetaEta where


-- |
-- Anything that can be expressed with θ- and η- coodinates.
-- We don't use another type for the parameters other than the distribution
-- itself. e.g.  We will use @Normal μ σ@ directry and
-- we don't use @(μ, σ)@.
class ThetaEta a where
    -- |
    -- @TEParam dist@ is the type for θ- and η- coordinates.
    -- For distributions with 2 degrees of freedom, we'll prefer
    -- @(Double, Double)@.
    type TEParam a
    -- | Get the θ coordinates.
    toθ :: a -> Theta a
    -- | Given θ, returns the distribution.
    fromθ :: Theta a -> a
    -- | Get the η coordinates.
    toη :: a -> Eta a
    -- | Given η, returns the distribution.
    fromη :: Eta a -> a
    -- | Returns a new distribution with θ modified as instructed.
    modifyθ :: (TEParam a -> TEParam a) -> a -> a
    modifyθ f = fromθ . θmap f . toθ
    -- | Returns a new distribution with η modified as instructed.
    modifyη :: (TEParam a -> TEParam a) -> a -> a
    modifyη f = fromη . ηmap f . toη


-- | @Theta a@ and @Eta a@ contains the same thing, usually n Doubles,
-- and we'll use newtypes to make our life a little more type-safe.
data Theta a = (ThetaEta a) => Theta (TEParam a)
θmap :: (ThetaEta a, ThetaEta b) => (TEParam a -> TEParam b) -> Theta a -> Theta b
θmap f (Theta a) = Theta (f a)
unθ :: ThetaEta a => Theta a -> TEParam a
unθ (Theta θ) = θ
data Eta a = (ThetaEta a) => Eta (TEParam a)
ηmap :: (ThetaEta a, ThetaEta b) => (TEParam a -> TEParam b) -> Eta a -> Eta b
ηmap f (Eta a) = Eta (f a)
unη :: ThetaEta a => Eta a -> TEParam a
unη (Eta η) = η
