module Three where

data Three a b c = Three a b c 
  deriving (Eq, Show)

instance Semigroup (Three a b c) where
  (Three a b c) <> (Three d e f) = 
    Three a e f

-- (Three a b c) <> (Three a' b' c') <> (Three a'' b'' c'')
