module Identity where


newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

{-
The identity for 'Identity a' should be the identity for the concrete instance of 'a'
So the mempty for 'Monoid (Identity a)' must trivially be 'Identity mempty'

-}

p = Identity "a"
q = Identity "b"
r = Identity "c"

main :: IO ()
main = do
  print $ p <> (q <> r)
  print $ (p <> q) <> r
  print $ p <> (q <> r) == (p <> q) <> r
  print $ p <> mempty == p
  print $ mempty <> p == p

{-
Identity "abc"
Identity "abc"
True
True
True
-}

