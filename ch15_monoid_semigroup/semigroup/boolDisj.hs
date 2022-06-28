module BoolDisj where

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> (BoolDisj _) = BoolDisj True
  (BoolDisj _) <> (BoolDisj True) = BoolDisj True
  _ <> _ = BoolDisj False