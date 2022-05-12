module Folds where

myOr :: [Bool] -> Bool
myOr = foldr (\x acc -> if x == True then True else acc) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> f x || acc) False

myElem :: Eq a => a -> [a] -> Bool 
myElem z = any (==z)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) [] 

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) [] 

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x acc -> case f x acc of 
                                      GT -> x
                                      EQ -> acc
                                      LT -> acc) (last xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\x acc -> case f x acc of
                                      GT -> acc
                                      EQ -> x
                                      LT -> x) (last xs) xs