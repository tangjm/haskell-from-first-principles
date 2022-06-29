module Question2 where

import Data.List (sort)
import Test.QuickCheck

-- Here '(_, False)' is bound to the name 'status'

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)


listGen :: Gen [Int] 
listGen = arbitrary

-- arbitrary :: Arbitrary a => Gen a
-- quickCheck :: Testable prop => prop -> IO ()
-- forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property

testListSort :: Property
testListSort =
  forAll listGen (\xs -> (listOrdered . sort) xs) 


main :: IO ()
main = 
  quickCheck testListSort