module NumsAndInts where

data Nat = 
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = succ . natToInteger $ n

integerToNat' :: Integer -> Nat
integerToNat' n 
  | n == 0 = Zero
  | n > 0 = Succ (integerToNat' (n - 1))

integerToNat :: Integer -> Maybe Nat
integerToNat n 
  | n < 0  = Nothing
  | n == 0 = Just Zero
  | n > 0  = Just (integerToNat' n)
