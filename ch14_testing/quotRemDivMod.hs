module QuotRemDivMod where

import Test.QuickCheck

isPositive :: Int -> Bool
isPositive = (> 0)

isNegative :: Int -> Bool
isNegative = (< 0)

ignoreDivisionByZero :: (Int -> Int -> Bool) -> Int -> Int -> Maybe Bool
ignoreDivisionByZero f x y
  | isPositive y || isNegative y = Just (f x y)
  | otherwise                    = Nothing


testQuotAndRem :: Int -> Int -> Bool
testQuotAndRem x y =
  (quot x y) * y + (rem x y) == x 

testDivAndMod :: Int -> Int -> Bool
testDivAndMod x y =
  (div x y) * y + (mod x y) == x

main :: IO ()
main = do
  quickCheck $ ignoreDivisionByZero testQuotAndRem
  quickCheck $ ignoreDivisionByZero testDivAndMod

-- Fix 'division by zero error