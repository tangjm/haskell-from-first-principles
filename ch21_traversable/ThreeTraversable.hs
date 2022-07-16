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


data Three' a b =
  Three' a b b 

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)


-- instance Foldable (Three' a) where
--   foldMap f (Three' x y z) = f z
--   foldr f a (Three' x y z) = f z a 

-- instance Foldable (Three' a) where 
--   foldMap f (Three' x y z) = f y
--   foldr f a (Three' x y z) = f y a

instance Foldable (Three' a) where
  foldMap f (Three' x y z) = f y <> f z
  foldr f a (Three' x y z) = f y (f z a)

-- n.b. 'f' is not only binary but also associative 
-- There seems to be more than one way to implement the Foldable typeclass

instance Traversable (Three' a) where
  traverse f (Three' x y z) = Three' x <$> (f y) <*> (f z)
  
