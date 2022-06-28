module QuickCheckExamples where

import Test.QuickCheck

arbitraryInt :: Gen Int
arbitraryInt = arbitrary

trivialInt :: Gen Int
trivialInt = return 1

-- While 'arbitraryInt' is a generator for integers, 'trivialInt' is a generator for the constant value, '1'.

oneThroughThree :: Gen Int
oneThroughThree = elements [1..3]

oneThroughThree' :: Gen Int
oneThroughThree' = elements [1, 2, 2, 2, 2, 3]

-- The 'elements' function lets you specify a list of possible values that a generator can generate.
-- In this case, 'oneThroughThree' will return a generator that generates values from the list '[1..3]'.
-- Including duplicate elements increases their probability of showing up

