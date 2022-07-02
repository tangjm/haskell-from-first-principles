module TakeN where

import Test.QuickCheck

f :: Int -> [a] -> Bool
f n xs = length (take n xs) == n

f' :: Int -> [Int] -> Bool
f' = f 

main :: IO ()
main = do
  quickCheck f'