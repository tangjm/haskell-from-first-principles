module TypeclassInstances where 

-- 1
data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  TisAn n == TisAn m = n == m

-- 2
data TwoIntegers = 
  Two Integer Integer

instance Eq TwoIntegers where
  Two m n == Two m' n' = m == m' && n == n'


-- 3
data StringOrInt =
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  TisAnInt v == TisAnInt v' = v == v'
  TisAString v == TisAString v' = v == v'
  TisAnInt v == TisAString v' = False
  TisAString v == TisAnInt v' = False

-- 4
data Pair a =
  Pair a a

instance Eq a => Eq Pair a where
  Pair v v == Pair v' v' = v == v'

-- 5
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq Tuple a b where
  Tuple v w == Tuple v' w' = v == v && w == w'


-- 6
data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq Which a =
  ThisOne v == ThisOne v' = v == v'
  ThatOne v == ThatOne v' = v == v'
  ThisOne v == ThatOne v' = False
  ThatOne v == ThisOne v' = False

-- 7
data EitherOr a b =
    Hello a 
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello v == Hello v' = v == v'
  Goodbye v == Goodbye v' = v == v'
  Hello v == Goodbye v' = False 
  Goodbye v == Hello v' = False
  -- _ == _ = False
