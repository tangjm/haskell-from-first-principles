module QuotRemDivMod where

import Test.QuickCheck

isPositive :: Int -> Bool
isPositive = (> 0)

isNegative :: Int -> Bool
isNegative = (< 0)

-- We don't run tests for when the divisor is 0
ignoreDivisionByZero :: (Int -> Int -> Bool) -> Int -> Int -> Maybe Bool
ignoreDivisionByZero f x y
  | isPositive y || isNegative y = Just (f x y)
  | otherwise                    = Nothing


positiveIntGen :: Gen Int
positiveIntGen = elements [1..]

negativeIntGen :: Gen Int
negativeIntGen = elements [-1, -2 ..]

intGen :: Gen Int
intGen = frequency [ (1, positiveIntGen),
                         (1, negativeIntGen) ]

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

  -- The following computation won't ever end because we have unbounded quantification.
  -- quickCheck $ forAll intGen 
  --               (\x -> forAll intGen 
  --                 (\y -> testQuotAndRem x y))

-- Fix 'division by zero error

