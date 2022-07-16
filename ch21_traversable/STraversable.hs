module STraversable where

data S n a = S (n a) a 

instance Functor n => Functor (S n) where
  fmap f (S x y) = S (fmap f x) (f y)

instance Foldable n => Foldable (S n) where
  foldMap f (S x y) = foldMap f x <> f y 

instance Traversable n => Traversable (S n) where
  traverse f (S x y) = S <$> traverse f x <*> f y

-- n.b. it's interesting how the Traversable constraint on 'n' in the Traversable instance constrains how we must
-- implement the Functor and Foldable instances.

{- 
S x y where x = n a and y = a

traverse f (S x y)
S <$> traverse f x <*> f y
let (f x) = g x'
let (f y) = g y'
S <$> g x' <*> g y'
g (S x') <*> g y'
g (S x' y') 

n.b. x' is a Traversable

-} 