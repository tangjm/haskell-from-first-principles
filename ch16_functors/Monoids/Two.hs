module Two where

import Data.Monoid

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two x y <> Two x' y' = Two (x <> x') (y <> y')

-- instance Semigroup (Two a b) where
--   Two x y <> Two x' y' = Two x y

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

p = Two "a" "b"
q = Two "c" "d"
r = Two "e" "f"

main :: IO ()
main = do
  print $ (p <> q) <> r
  print $ p <> (q <> r)
  print $ p <> mempty == p
  print $ mempty <> p == p

