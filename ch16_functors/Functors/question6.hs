{-# LANGUAGE FlexibleInstances #-}
module Question6 where

data Parappa f g a = 
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where 
  fmap h (DaWrappa (f a) (g a)) = Parappa (f (h a)) (g (h a))