module Four where

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') =
    Four a b c' d'