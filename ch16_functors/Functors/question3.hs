{-# LANGUAGE FlexibleInstances #-}
module Question3 where

-- 3
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show) 

newtype K a b = 
  K a  

instance Functor (Flip K a) where 
  fmap f (Flip (K a)) = Flip $ K (f a)