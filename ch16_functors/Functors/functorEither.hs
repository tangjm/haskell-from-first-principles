module Sum where

data Sum a b = 
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First e) = First e
  fmap f (Second a) = Second (f a)