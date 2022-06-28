{-# LANGUAGE TupleSections #-}

module Mem where

newtype Mem s a = 
  Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g = 
    Mem $ \x -> ((fst . g) x <> (fst . f . snd . g) x, (snd . f . snd . g) x)

instance Monoid a => Monoid (Mem s a) where
  -- mempty = Mem $ \x -> (mempty, x)
  mempty = Mem (mempty, )
  mappend = (<>)

f' = Mem $ \s -> ("Hi", s + 1)

main = do
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0

{-
("Hi", 1)
("Hi", 1)
("", 0)
True
True
-}

-- id :: s -> (mempty, s)
-- How do we compose (Mem s a)?
{-
Mem f <> Mem g = Mem $ \x -> f x <> g x
But here 'f x' and 'g x' are tuples.
f x <> g x == (a, s) <> (a', s')
(a, s) <> (a', s') == (a <> a', ???)

This means we need (,) and a to be Monoids.
f :: s -> (a, s)
g :: s -> (a, s)


Mem f <> Mem g = 
  Mem $ \x -> ((fst . g) x <> (fst . f . snd . g) x, (snd . f . snd . g) x)

-}

