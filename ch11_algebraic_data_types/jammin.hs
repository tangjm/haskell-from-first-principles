module Jammin where

import Data.List

data Fruit = 
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Ord, Show)

-- data JamJars = 
--   Jam Fruit Int 
--   deriving (Eq, Show)
data JamJars = 
  Jam { fruit :: Fruit, quantity :: Int }
  deriving (Eq, Ord, Show)


data Cool = Cool { level :: Int } deriving (Eq, Show)
-- we can use record syntax for unary data constructors

row1 = Jam Peach 20
row2 = Jam Plum 20
row3 = Jam Peach 20
row4 = Jam Peach 20
row5 = Jam Apple 22
row6 = Jam Blackberry 20
allJam = [row1, row2, row3, row4, row5, row6]

totalJars :: [JamJars] -> Int
totalJars = foldr ((+) . quantity) 0 

mostJars :: [JamJars] -> JamJars
mostJars xs = foldr (\x acc -> if quantity x > quantity acc then x else acc) (head xs) xs
-- find a better implementation

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

sortByFruit :: [JamJars] -> [JamJars]
-- sortByFruit = sortBy (\x y -> compare (fruit x) (fruit y))
sortByFruit = sortBy compareKind

groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy (\x y -> fruit x == fruit y) . sortByFruit
