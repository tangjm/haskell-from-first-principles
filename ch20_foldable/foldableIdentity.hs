module FoldableIdentity where

data Identity a =
  Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x 


-- All the above are equivalent, since the 'z' is the identity for the Identity monoid.
-- With 'foldMap', there is nothing to fold a binary associative function over. 