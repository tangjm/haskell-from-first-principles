module TreeTraversable where 

data Tree a =
    Empty
  | Leaf a 
  | Node (Tree a) a (Tree a)

instance Functor Tree where 
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right) 

instance Foldable Tree where 
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node left x right) = foldMap f left <> f x <> foldMap f right

instance Traversable Tree where 
  traverse f Empty = pure Empty 
  traverse f (Leaf x) = Leaf <$> f x 
  traverse f (Node left x right) = Node <$> traverse f left <*> f x <*> traverse f right 
