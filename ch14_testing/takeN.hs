module TakeN where

import Test.QuickCheck

f :: Int -> [a] -> Bool
f n xs = length (take n xs) == n

f' :: Int -> [Int] -> Bool
f' = f 

main :: IO ()
main = do
  quickCheck f'
  -- quickCheck f - this doesn't work as quickCheck would not know what to generate for the second argument to function f. The 'a' in '[a]' needs to be a concrete type.