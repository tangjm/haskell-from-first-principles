module Question5 where

data LiftItOut f a =
  LiftItOut (f a)

-- instance Functor (f a) where
--   fmap g (f a) = f (g a)

instance Functor f => Functor (LiftItOut f) where 
  fmap g (LiftItOut (f a)) = LiftItOut $ fmap g (f a)