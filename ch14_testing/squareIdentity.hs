module SquareIdentity where

import Test.QuickCheck

square :: Num a => a -> a
square x = x * x

-- sqrt :: Floating a -> a
squareIdentity = square . sqrt

squareIdentity' :: Float -> Bool
squareIdentity' x = squareIdentity x == x

main :: IO ()
main = do
  quickCheck squareIdentity'