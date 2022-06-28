module Trivial where

-- import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = do
  print $ (Trivial <> Trivial) <> Trivial
  print $ Trivial <> (Trivial <> Trivial)
  print $ Trivial <> mempty == Trivial
  print $ mempty <> Trivial == Trivial

-- main :: IO ()
-- main = do 
--   quickCheck (semigroupAssoc :: TrivialAssoc)
--   quickCheck (monoidLeftIdentity :: Trivial -> Bool)
--   quickCheck (monoidRightIdentity :: Trivial -> Bool)
