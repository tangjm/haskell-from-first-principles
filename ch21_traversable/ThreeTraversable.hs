module ThreeTraversable where

data Three a b c = 
  Three a b c 

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z

  foldr f a (Three x y z) = f z a 

instance Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> f z 
