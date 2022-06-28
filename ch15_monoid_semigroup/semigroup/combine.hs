module Combine where 

import Data.Monoid (Sum)

newtype Combine a b =
  Combine { unCombine :: (a -> b) }
  
instance Semigroup b => Semigroup (Combine a b) where 
  (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)
    
  
-- f :: Num a => a -> Sum a
-- g :: Num a => a -> Sum a

-- Combine f :: Num a => Combine a (Sum a)
-- Combine g :: Num a => Combine a (Sum a)

-- (<>) :: Num a => Combine a (Sum a) 
--               -> Combine a (Sum a) 
--               -> (a -> Sum a)

-- (Combine f) <> (Combine g) :: Num a => a -> Sum a 

-- (unCombine $ (Combine f) <> (Combine g)) $ x = (f x <> g x)

-- (Combine f) <> (Combine g) = (\x -> f x <> g x)

-- (Combine (a -> b)) <> (Combine (a' -> b')) =
--   (a -> b) <> (a' -> b') =
--     b <> b'  