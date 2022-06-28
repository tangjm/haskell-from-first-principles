module Or where

data Or a b =
    Fst a
  | Snd b  
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd b) <> _ = Snd b
  (Fst a) <> y = y