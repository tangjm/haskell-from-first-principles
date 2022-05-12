{-# LANGUAGE FlexibleInstances #-}

module Newtype where 


class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where 
  tooMany n = n > 42

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43

-- n.b. you can define typeclass instances for newtype that differ from typeclass instances defined for its contained types, namely, Int, in this example. Compare the typeclass instance for Int and Goats

instance TooMany (Int, String) where
  tooMany (n, xs) = length xs == n

instance TooMany (Int, Int) where
  tooMany (n, m) = n + m > 42

