module Idempotence where

import Test.QuickCheck
import Data.List 

twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes = twice . twice

-- f x = 
--   capitalizeWord x 
--   == twice capitalizeWord x 
--   == fourTimes capitalizeWord x

g :: (Ord a) => [a] -> Bool
g x =
  sort x == twice sort x
  && 
  sort x == fourTimes sort x

testSortInts :: [Int] -> Bool
testSortInts = g

main :: IO ()
main = do
  quickCheck testSortInts