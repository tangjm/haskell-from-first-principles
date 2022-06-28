module CombineMonoidInstance where

newtype Combine a b = 
  Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine $ \x -> mempty
  mappend = (<>)
  
{-

A concrete example.

let f = Combine $ \x -> Sum (x + 1)
let g = Combine $ \x -> Sum (x - 1)
let h = Combine $ \x -> f x <> g x
      = Combine $ \x -> Sum (x + 1) <> Sum (x - 1)

The plan.

We need to find some (Combine f) s.t.
(Combine f) <> (Combine g) = Combine g
(Combine g) <> (Combine f) = Combine g

Put differently, we need some function f s.t.
f x <> g x == g x
g x <> f x == g x

(<>) will need to be defined for 'f x'.
'f x' has type 'b' and will need to be the identity of the data type 'b'.
'b' will need to be a Monoid.

f :: Monoid b => a -> b
The unary data constructor of a Monoid wrapper seems to be a good candidate.

For the Sum data type,
f' :: (Num a, Num b, Monoid b) => a -> b
f' = \x -> Sum 0

More examples
f'' = \x -> Product 1
f''' = \x -> BoolConj True
f'''' = \x -> BoolDisj False

Our function just needs to generalise these functions.
We want a function that constructs the identity of a type and then passes it as an argument to an unary Monoid data constructor.

Our function simply needs to return the identity of a Monoid. What value 'mempty' has is then dependent on the other operand to (<>).
f = \x -> mempty
should suffice as the argument to our (Combine a b) data constructor.

Finally, this gives Combine (\x -> mempty)


-}