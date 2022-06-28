module Composition where

import Data.Monoid

newtype Comp a = Comp { deComp:: a -> a }

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f . g)

instance Monoid a => Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

{-
Example - A wrapper for the successor function defined for $\mathbb{N}$

f = Comp $ \x -> Sum 1 <> x
h = f <> mempty
h' = mempty <> f

deComp h $ Sum 2 == deComp h' $ Sum 2
-}