module SemigroupInstances where

data Validation a b = 
    Failure a
  | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where 
  (Success b) <> (Success b') = Success b
  (Failure a) <> (Failure a') = Failure (a <> a')
  (Failure a) <> (Success b) = Failure a
  (Success b) <> (Failure a) = Failure a

{-
If both operands are (Success b, we return the left operand
If one operand is (Success b), we return that operand
If both operands are (Failure a), we return (Failure $ a <> a)
-}

newtype AccumulateRight a b = 
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Success x) <> AccumulateRight (Success y) = AccumulateRight $ Success (x <> y)
  AccumulateRight (Success x) <> AccumulateRight (Failure y) = AccumulateRight $ Success x
  AccumulateRight (Failure x) <> AccumulateRight (Success y) = AccumulateRight $ Success y
  AccumulateRight (Failure x) <> AccumulateRight (Failure y) = AccumulateRight $ Failure y


{-
If one operand is a (AccumulateRight Failure a) then we return the (Accumulateright Success b)
If both operands are (AccumulateRight Failure a) then we return the right operand
If both operands are (AccumulateRight Success b) then we apply the (<>) operator to the two (Success b) and return the result of applying AccumulateRight to that result.
-}

newtype AccumulateBoth a b = 
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Success x) <> AccumulateBoth (Success y) = AccumulateBoth $ Success (x <> y)
  AccumulateBoth (Success x) <> AccumulateBoth (Failure y) = AccumulateBoth $ Failure y
  AccumulateBoth (Failure x) <> AccumulateBoth (Success y) = AccumulateBoth $ Failure x
  AccumulateBoth (Failure x) <> AccumulateBoth (Failure y) = AccumulateBoth $ Failure (x <> y)

{- 
If both are (Success b), return the result of applying the (<>) to the type arguments of both operands, namely (Success $ b <> b)
If one is (Success b), either return that operand or the other (Failure a) operand.
If both are (Failure a), return the result of applying the (<>) to the type arguments of both operands, namely (Failure $ a <> a)
-}