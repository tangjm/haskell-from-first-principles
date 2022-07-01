module Exponentiation where

import Test.QuickCheck

associativity :: Int -> Int -> Int -> Bool
associativity x y z = 
  x ^ (y ^ z) == (x ^ y) ^ z

commutativity :: Int -> Int -> Bool
commutativity x y =
  x ^ y == y ^ x

main :: IO ()
main = do
  quickCheck associativity
  quickCheck commutativity