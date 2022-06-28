module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

mult :: (Eq a , Num a) => a -> a -> a 
mult x y 
  | y == 0    = y
  | otherwise = (+) (mult x (y - 1)) x


-- Generators created form `choose` and `elements`

genBool :: Gen Bool  
genBool = choose (False, True)

genBool' :: Gen Bool 
genBool' = elements [False, True]

genOrdering :: Gen Ordering 
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char  
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genTriple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genTriple = 
  arbitrary >>= 
    \a -> arbitrary >>=
      \b -> arbitrary >>=
        \c -> return (a, b, c)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)

  describe "Multiplication" $ do
    it "0 times 2 is 0" $ do
      mult 0 2 `shouldBe` 0
    
    it "2 times 3 is 6" $ do
      mult 2 3 `shouldBe` 6
    
    it "3 times 0 is 0" $ do
      mult 3 0  `shouldBe` 0

  describe "QuickCheck tests" $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

-- We can use QuickCheck without Hspec as follows:

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

prop_additionGreaterFaulty :: Int -> Bool
prop_additionGreaterFaulty x = x + 0 > x

runQc :: IO ()
runQc = 
  quickCheck prop_additionGreater >>
  quickCheck prop_additionGreaterFaulty