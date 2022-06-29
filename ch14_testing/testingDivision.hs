module TestingDivision where

import Test.QuickCheck

half :: Fractional a => a -> a
half x = x / 2  

doubleGen :: Gen Double 
doubleGen = arbitrary 

floatGen :: Gen Float  
floatGen = arbitrary 

halfIdentity = (*2) . half 

-- This test doesn't make sense as 'Fractional' is a typeclass, not a type
-- testForIdentityFractional :: Property
-- testForIdentityFractional = forAll fractionalGen (\n -> n == halfIdentity n)

-- testForIdentityFloat :: Property
-- testForIdentityFloat = 
--   forAll floatGen
--   (\n -> n == halfIdentity n)

testForIdentityDouble :: Property
testForIdentityDouble = 
  forAll doubleGen
  (\n -> n == halfIdentity n)

main :: IO ()
main = 
  quickCheck testForIdentityDouble