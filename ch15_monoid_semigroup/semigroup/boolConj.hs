module BoolConj where

newtype BoolConj = BoolConj Bool 
  deriving (Eq, Show)

instance Semigroup BoolConj where 
  (BoolConj False) <> (BoolConj _) = BoolConj False
  (BoolConj _) <> (BoolConj False) = BoolConj False
  _ <> _ = BoolConj True


