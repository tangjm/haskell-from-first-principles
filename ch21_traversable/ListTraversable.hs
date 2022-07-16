module ListTraversable where

import Control.Applicative

data List a =
    Nil  
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = (f x) <> (foldMap f xs)

  foldr _ a Nil = a
  foldr f a (Cons x xs) = f x (foldr f a xs)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs
  -- traverse f (Cons x xs) = liftA2 Cons (f x) (traverse f xs)

-- n.b. Cons has arity 2

{- An example of how this works

traverse f (Cons x (Cons x' (Cons x'' Nil)))
Cons <$> (f x) <*> traverse f (Cons x' (Cons x'' Nil))
Cons <$> (f x) <*> Cons <$> (f x') <*> traverse f (Cons x'' Nil)
Cons <$> (f x) <*> Cons <$> (f x') <*> Cons <$> (f x'') <*> traverse f Nil
Cons <$> (f x) <*> Cons <$> (f x') <*> Cons <$> (f x'') <*> pure Nil

let (f x) = g y 
let (f x') = g y' 
let (f x'') = g y''

Cons <$> (f x) <*> Cons <$> (f x') <*> g (Cons x'') <*> pure Nil
Cons <$> (f x) <*> Cons <$> (f x') <*> g (Cons x'' Nil)
Cons <$> (f x) <*> g (Cons x') <*> g (Cons x'' Nil)
Cons <$> (f x) <*> g (Cons x' (Cons x'' Nil))
g (Cons x) <*> g (Cons x' (Cons x'' Nil))
g (Cons x (Cons x' (Cons x'' Nil)))

-}



