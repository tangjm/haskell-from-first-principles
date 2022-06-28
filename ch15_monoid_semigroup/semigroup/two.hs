module Two where

data Two a b = Two a b 
  deriving (Eq, Show)

instance Semigroup (Two a b) where
  -- (Two x y) <> (Two x' y') = Two x y'
  (Two x y) <> (Two x' y') = Two x' y


-- (Two x y) <> (Two x' y') <> (Two x'' y'')