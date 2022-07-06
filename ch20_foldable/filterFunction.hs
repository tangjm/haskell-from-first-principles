module FilterFunction where

filterF :: (Applicative f, Foldable f, Monoid (f a)) => 
          (a -> Bool) -> f a -> f a 

-- <*> :: f (a -> b) -> f a -> f b
-- foldMap :: (a -> m) -> t a -> m

filterF p xs = 
  foldMap (\x -> if p x then pure x else mempty) xs

{- 
An example with lists: foldMap even [1..5]

The 'map' part of 'foldMap' will produce [[], [2], [], [4], []]
Then the 'fold' part will fold `mappend` over it, which, for lists, is the (++) operator. 
So we get [2, 4].
-}

{-
We want our foldMap to have this type signature: 
foldMap :: (a -> f a) -> f a -> f a

The idea is to use a characteristic function that enscapsulates the filter with type 'a -> Bool' and maps 'f a' to 'f (f a)' and then fold the 'f (f a)' which will produce a value with type 'f a'.

Because 'pure' wraps the 'a' into an Applicative 'f a' and we know that it is also a Monoid from our 'Monoid (f a)' type constraint, we can be certain that it will work with the `mappend` that will later be used as part of the 'fold' of 'foldMap'.

Because `mempty` is the identity of a Monoid, it will be ignored by the `mappend`.

So, with this in mind, we can wrap values we want to keep using `pure` and use `mempty` to denote those values we want to filter out.

-}
