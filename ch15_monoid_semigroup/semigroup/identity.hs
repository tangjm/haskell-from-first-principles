module Identity where

newtype Identity a = Identity a  
  deriving (Eq, Show)

instance Semigroup (Identity a) where
  x <> y = x