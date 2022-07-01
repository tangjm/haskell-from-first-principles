module FoldrTests where

import Test.QuickCheck

testListAppend :: Eq a => [a] -> [a] -> Bool
testListAppend xs ys =
  foldr (:) xs ys == (++) ys xs

{- 
For example.
foldr (:) "world" "hello " == "hello " ++ "world" == "hello world"
-}

testListConcatenation :: Eq a => [[a]] -> Bool
testListConcatenation xs = 
  foldr (++) [] xs == concat xs


testListAppendInt :: [Int] -> [Int] -> Bool
testListAppendInt = testListAppend

testListConcatenationInt :: [[Int]] -> Bool
testListConcatenationInt = testListConcatenation


main :: IO ()
main = do
  quickCheck testListAppendInt
  quickCheck testListConcatenationInt