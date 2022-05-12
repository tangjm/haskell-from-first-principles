module EnumDefs where

eftBool :: Bool -> Bool -> [Bool]
-- Not so naive implementation?
-- eftBool True True = [True]
-- eftBool True False = []
-- eftBool False True = [False, True]
-- eftBool False False = [False]

-- Recursive implementation
eftBool p q = go p q []
  where go p q boolList
          | p > q     = []
          | p == q    = p : boolList
          | otherwise = go p (pred q) (q : boolList)


eftOrd :: Ordering -> Ordering -> [Ordering]
-- Naive pattern matching implementation
-- eftOrd LT LT = [LT]
-- eftOrd LT EQ = [LT, EQ]
-- eftOrd LT GT = [LT, EQ, GT]
-- eftOrd EQ EQ = [EQ]
-- eftOrd EQ GT = [EQ, GT]
-- eftOrd EQ LT = []
-- eftOrd GT GT = [GT]
-- eftOrd GT EQ = []
-- eftOrd GT LT = []

-- Recursive implmentation
eftOrd x y = go x y []
  where go x y list
          | x > y  = []
          | x == y = x : list
          | otherwise = go x (pred y) (y : list)


eftInt :: Int -> Int -> [Int]
eftInt n m = go n m []
  where go n m enumList 
          | m < n     = []
          | m == n    = m : enumList
          | otherwise = go n (m - 1) (m : enumList)

eftChar :: Char -> Char -> [Char]
eftChar x y = go x y []
  where go x y charList
          | x > y     = []
          | x == y    = x : charList
          | otherwise = go x (pred y) (y : charList)