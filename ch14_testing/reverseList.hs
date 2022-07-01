module ReverseList where

import Test.QuickCheck

reverseTwice :: [a] -> [a]
reverseTwice = reverse . reverse

test :: Eq a => [a] -> Bool
test xs = reverseTwice xs == id xs

testIntList :: [Int] -> Bool
testIntList = test

testCharList :: [Char] -> Bool
testCharList = test

main :: IO ()
main = do
  quickCheck testIntList
  quickCheck testCharList
