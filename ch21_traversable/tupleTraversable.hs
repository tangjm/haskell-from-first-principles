module TupleTraversable where

data (,) a b = (,) a b 

instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)

-- Note that the first element of an ordered pair is part of the structure of (,) a

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)

-- Create a tuple from a single value

  (u, f) <*> (v, x) = (u <*> v, f x) 

  -- Since we know the first element of a tuple is a monoid, we can combine them using `mappend`/<*> 

instance Foldable ((,) a) where
  foldMap f (_, y) = f y
  foldr f z (_, y) = f y z 

instance Traversable ((,) a) where
  traverse f (x, y) = (,) x <$> f y


-- Consider why this is wrong,
-- traverse f ((,) x y) -> (,) x (f y)