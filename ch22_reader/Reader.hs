module Reader where

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  -- fmap :: (a -> b) -> Reader r a -> Reader r b 
  -- fmap f (Reader g) = Reader $ \x -> f (g x)
  fmap f (Reader g) = Reader $ f . g

