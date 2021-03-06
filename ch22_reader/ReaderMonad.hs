{-# LANGUAGE InstanceSigs #-}

module ReaderMonad where

newtype Reader r a =
  Reader { runReader :: r -> a }
  
instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ f . g 

instance Applicative (Reader r) where
  pure x = Reader $ \y -> x

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b 
  Reader f <*> Reader g = Reader $ \x -> f x (g x)

instance Monad (Reader r) where
  return :: a -> Reader r a
  return x = pure x 

  -- >>= :: m a -> a -> m b -> m b
  -- >>= :: (r -> a) -> (a -> (r -> b)) -> r -> b 
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b 
  Reader f >>= g = Reader $ \x -> runReader (g (f x)) x 
  
  {- 
  Is there a more concise way of spelling this out?

  If it weren't for the Reader wrapper, we could define (>>=) in terms of (<*>).

  (<*>) :: (r -> a -> b) -> (r -> a) -> r -> b
  
  (>>=) :: (r -> a) -> (a -> r -> b) -> r -> b 
  f >>= g = flip g <*> f 
  -}

  -- The tricky part is the following reduction.
  -- Reader r (Reader r b) 
  -- Reader r b 

  -- r -> r -> b
  -- r -> b 

