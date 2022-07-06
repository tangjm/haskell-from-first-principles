module FoldableInstances where

data Constant a b =
  Constant a 

instance Foldable (Constant a) where
  foldr _ x (Constant a) = x
  -- foldMap f (Constant a) = undefined


data Two a b = 
  Two a b

instance Foldable (Two a) where
  foldr f x (Two a b) = f b x
  foldMap f (Two a b) = f b
  -- 'f b' must be a Monoid in 'foldMap'


data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldr f x (Three a b c) = f c x
  foldMap f (Three a b c) = f c


data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldr f x (Three' a b c) = f b x
  -- foldr f x (Three' a b c) = f b (f c x)
  foldMap f (Three' a b c) = f b


data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldr f x (Four' a b c d) = f b (f c (f d x))
  foldMap f (Four' a b c d) = f d