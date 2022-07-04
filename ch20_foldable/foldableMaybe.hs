module FoldableMaybe where

import Data.Monoid

data Optional a = 
    Nada
  | Yep a 
  deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada = z  
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z 
  foldl f z (Yep x) = f z x 

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a


-- foldMap (+1) Nada 
x = foldMap (+1) Nada :: Sum Integer
-- Sum {getSum = 0}

-- The compiler can't infer the Monoid, we must specify it explicitly.

-- We could also specify the Monoid explicitly in our 'map' function with the following:
y = foldMap Sum Nada
-- Sum {getSum = 0}