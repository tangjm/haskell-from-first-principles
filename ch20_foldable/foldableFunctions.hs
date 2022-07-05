module FoldableFunctions where

import Test.QuickCheck
import Data.Monoid

-- Functions implemented in terms of 'foldMap' and 'foldr'

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = foldr (+) 0

-- How can we swap out the two sum' and sum'' functions for testing?

-- withTestFunc :: 

testSumFoldable :: (Eq a, Foldable t, Num a) => t a -> Bool
testSumFoldable xs = sum' xs == sum xs

testSumList :: [Int] -> Bool
testSumList = testSumFoldable

testSumMaybe :: Maybe Int -> Bool
testSumMaybe = testSumFoldable

testSumTuple :: (Char, Int) -> Bool
testSumTuple = testSumFoldable

testSumEither :: Either Char Int -> Bool
testSumEither = testSumFoldable

testSumSum :: Sum Int -> Bool
testSumSum = testSumFoldable

testSumProduct :: Product Int -> Bool
testSumProduct = testSumFoldable

-- Returns the product of elements in a foldable 
product' :: (Foldable t, Num a) => t a -> a 
product' = getProduct . foldMap Product

-- Two implementations of 'elem' in terms of 'foldMap' and 'foldr'
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x xs = getAny $ foldMap (\y -> Any $ y == x) xs

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' x xs = getAny $ foldr (\y z -> Any (y == x) <> z) mempty xs

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = undefined
-- minimum' xs = Just $ foldr (\x y -> case compare x y of 
--                               LT -> x
--                               _  -> y)  xs


-- 8.
toList' :: Foldable t => t a -> [a]
toList' = foldMap (:[])

-- 9. 
fold' :: (Foldable t, Monoid m) => t m -> m 
fold' = foldMap id


-- 10. Define foldMap in terms of foldr.
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f xs = foldr (\x y -> f x <> y) mempty xs

testMyFoldMap :: (Foldable t, Monoid m, Eq m) => (a -> m) -> t a -> Bool
testMyFoldMap f xs = foldMap' f xs == foldMap f xs

-- We have to test different combinations of functions that return a monoid and foldable structures
-- With the following, we're holding the map function of 'foldMap' fixed so it always returns a list while varying the foldable.

testMyFoldMapList :: [Int] -> Bool
testMyFoldMapList = testMyFoldMap (:[])

testMyFoldMapMaybe :: Maybe Int -> Bool
testMyFoldMapMaybe = testMyFoldMap (:[])

testMyFoldMapTuple :: (Char, Int) -> Bool
testMyFoldMapTuple = testMyFoldMap (:[])

testMyFoldMapEither :: Either Char Int -> Bool
testMyFoldMapEither = testMyFoldMap (:[])

testMyFoldMapSum :: Sum Int -> Bool
testMyFoldMapSum = testMyFoldMap (:[])

testMyFoldMapProduct :: Product Int -> Bool
testMyFoldMapProduct = testMyFoldMap (:[])


main :: IO ()
main = do
  putStrLn "Tests for 'sum'"
  quickCheck $ testSumList
  quickCheck $ testSumMaybe
  quickCheck $ testSumTuple
  quickCheck $ testSumEither
  quickCheck $ testSumSum
  quickCheck $ testSumProduct

  putStrLn "Tests for 'foldMap'"
  quickCheck $ testMyFoldMapList
  quickCheck $ testMyFoldMapMaybe
  quickCheck $ testMyFoldMapTuple
  quickCheck $ testMyFoldMapEither
  quickCheck $ testMyFoldMapSum
  quickCheck $ testMyFoldMapProduct