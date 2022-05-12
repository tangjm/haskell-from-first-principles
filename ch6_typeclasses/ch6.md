```haskell
write the Eq instance for the following datatypes.

1. 
data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  TisAn n == TisAn m = n == m

2.
data TwoIntegers = 
  Two Integer Integer

instance Eq TwoIntegers where
  Two m n == Two m' n' = m == m' && n == n'

3. 
data StringOrInt =
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  TisAnInt v == TisAnInt v' = v == v'
  TisAString v == TisAString v' = v == v'
  TisAnInt v == TisAString v' = False
  TisAString v == TisAnInt v' = False

4.
data Pair a =
  Pair a a

instance Eq a => Eq Pair a where
  Pair v v == Pair v' v' = v == v'

5.
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq Tuple a b where
  Tuple v w == Tuple v' w' = v == v && w == w'

6.
data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq Which a where
  ThisOne v == ThisOne v' = v == v'
  ThatOne v == ThatOne v' = v == v'
  ThisOne v == ThatOne v' = False
  ThatOne v == ThisOne v' = False

7.
data EitherOr a b =
    Hello a 
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello v == Hello v' = v == v'
  Goodbye v == Goodbye v' = v == v'
  Hello v == Goodbye v' = False 
  Goodbye v == Hello v' = False

```

6.14 Chapter Exercises

1c
2b
3a
4c
5a

Does it typecheck?


```haskell
-- 1.
data Person = Person Bool

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- No because Person datatype doesn't implement typeclass Show

-- 2. 
data Mood = Blah
          | Woot deriving Show

settleDown x = if x == Woot
                  then Blah
                  else x
-- No, because Mood needs to implement or derive typeclass Eq

-- 3.
-- 3a. Concrete instances of Mood
-- 3b Compile error because the first operand to x == Woot determines the datatype for the (==) operator. And since x is 9 which is a Num, Woot will need to have an instance of the Num typeclass instance for 9 == Woot to compile.
-- 3. Compile error because datatype Mood doesn't implement an instance of typeclass Ord, for (>) is a method from the typeclass Ord.

-- 4.
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- yes, s1 is a partial application of the Sentence function
```


Given a datatype declaration, what can we do?

```haskell
data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

1. phew = Papu "chases" True
No, because "chases" isn't a function

2. truth = Papu (Rocks "chomskydoz") (Yeah True)
yes

3. equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'
yes

4. comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
-- no, Papu datatype doesn't have an instance of the Ord typeclass.
```

Match the types


typeKwon-Do - round 2


```haskell
-- 1. tests whether the first argument applied to the second argument gets you the third argument
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

2. 
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f n x = f x

```

