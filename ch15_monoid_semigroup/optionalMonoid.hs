module OptionalMonoid where

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)


instance Semigroup a => Semigroup (Optional a) where
  Nada <> y            = y
  x <> Nada            = x
  (Only a) <> (Only b) = Only (a <> b)

instance Monoid a => Monoid (Optional a) where
  mempty         = Nada
  -- mappend Nada y = y
  -- mappend x Nada = x
  -- mappend (Only a) (Only b) = Only (mappend a b)


-- Only (Sum 1) `mappend` Only (Sum 1)
-- Only (Sum 1 `mappend` Sum 1)
-- Only (Sum 2)

-- a(x) `f` a(y)
-- a (x `f` y)

