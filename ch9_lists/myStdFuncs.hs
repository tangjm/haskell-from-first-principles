module MyStdFunctions where
import System.Win32 (COORD(x), zeroMemory)


myAnd :: [Bool] -> Bool 
myAnd [] = True 
myAnd (x : xs) = x && myAnd xs

myOr :: [Bool] -> Bool 
myOr [] = False 
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool 
myAny f [] = False
myAny f (x : xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool 
-- myElem x [] = False 
-- myElem x (z : zs) = x == z || myElem x zs

myElem x = myAny (==x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (xs : xss) = xs ++ squish xss

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x : xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a

-- [1, 2, 3, 4] -> (1 : 3 : [4]) -> (1 : 4 : []) -> 1
myMaximumBy f (x : y : [])
    | f x y == GT = x
    | f x y == EQ = y
    | f x y == LT = y 
myMaximumBy f (x : y : xs)
    | f x y == GT = myMaximumBy f (x : head xs : tail xs)  
    | f x y == EQ = myMaximumBy f (y : head xs : tail xs)
    | f x y == LT = myMaximumBy f (y : head xs : tail xs)                      

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = undefined

