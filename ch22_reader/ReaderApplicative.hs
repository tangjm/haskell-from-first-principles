{-# LANGUAGE InstanceSigs #-}

module ReaderApplicative where

newtype Reader r a = 
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader r) where
  pure :: a -> Reader r a 
  pure y = Reader $ \x -> y

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b 
  Reader f <*> Reader g = Reader $ \x -> f x (g x) 

-- n.b. The 'InstanceSigs' lets us add type signatures for typeclass methods

{-
An example.

f = (+)
g = (*2)

h = Reader f <*> Reader g 

runReader h $ 2
6

h is a function that returns an input after doubling it and adding the result to itself.
-}

