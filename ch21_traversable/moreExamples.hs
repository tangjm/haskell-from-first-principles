module MoreExamples where

import Data.Functor.Identity
import Data.Functor.Constant
import Data.Monoid 

-- runIdentity $ traverse (Identity . (+1)) [1, 2]


-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

-- Functorial 'fmap' in terms of 'traverse'

fmap' :: (Traversable t) => (a -> b) -> t a -> t b
fmap' f t = runIdentity $ traverse (Identity . f) t 

-- The Identity monoid 

-- Foldable 'foldMap' in terms of 'traverse'

foldMap' :: (Traversable t, Monoid m) => (a -> m) -> t a -> m 
foldMap' f t = getConstant $ traverse (Constant . f) t

xs = [1, 2, 3, 4, 5] :: [Sum Integer]

-- How does the Identity and Constant functors affect the differing behaviour of these two functions?

main :: IO ()
main = do 
  print $ fmap' (+1) [1..5]
  print $ foldMap' (+1) xs


{- 
traverse (Constant . (+1)) xs

  [Constant (Sum 2) .. Constant (Sum 6)]
  Constant [Sum 2 .. Sum 6]
  Constant [Sum 20]

traverse Constant xs

  [Constant (Sum 1) .. Constant (Sum 5)]
  Constant [Sum 1 .. Sum 6]
  Constant [Sum 15]

The Constant monoid let's us map the function 'f' and then apply 'fold' 
-}