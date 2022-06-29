module AdditionProperties where

import Test.QuickCheck

associativity :: Int -> Int -> Int -> Bool
associativity x y z = 
  x + (y + z) == (x + y) + z

commutativity :: Int -> Int -> Bool
commutativity x y = 
  x + y == y + x

intGen :: Gen Int
intGen = arbitrary

-- There's likely a more concise way to write this
associativityTest :: Property
associativityTest =
  forAll intGen 
    (\x -> forAll intGen 
            (\y -> forAll intGen
                    (\z -> associativity x y z)))

commutativityTest :: Property
commutativityTest = 
  forAll intGen
    (\x -> forAll intGen
      (\y -> commutativity x y))

main :: IO ()
main = do
  quickCheck associativityTest
  quickCheck commutativityTest
  quickCheck associativity
  quickCheck commutativity