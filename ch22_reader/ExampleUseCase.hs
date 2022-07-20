module ExampleUseCase where

import Control.Applicative
import ReaderMonad

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName = 
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)


data Person =
  Person {
      humanName :: HumanName
    , dogName :: DogName
    , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
      dogsName :: DogName
    , dogAddress :: Address
  } deriving (Eq, Show)

-- n.b. the record syntax argument names have to be unique across all data type declarations within the file.

person :: Person
person =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Some address")

jared :: Person
jared = Person (HumanName "Jared")
               (DogName "NanNan")
               (Address "UK")

getDog :: Person -> Dog 
getDog x = Dog (dogName x) (address x)

getDog' :: Person -> Dog
getDog' = Dog <$> dogName <*> address

getDog'' :: Person -> Dog
getDog'' = liftA2 Dog dogName address

getDogR :: Reader Person Dog
getDogR = Dog <$> Reader dogName <*> Reader address

-- (DogName -> Address -> Dog) -> Reader Person DogName -> Reader Person Address

getDogR' :: Reader Person Dog
getDogR' = liftA2 Dog (Reader dogName) (Reader address)

getDogFromR :: Person -> Dog
getDogFromR = runReader getDogR

-- Now using the Reader newtype with a Monad instance.

-- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b 
-- (Person -> DogName) -> (DogName -> Person -> Dog) -> Person -> Dog 
getDogRM :: Reader Person Dog
-- getDogRM = Reader dogName >>= \dogName p -> Dog dogName (address p)
getDogRM = Reader dogName >>= \dogName -> Dog dogName <$> Reader address