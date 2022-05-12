module SumTypes where

import Data.Int 

data BigSmall = 
    Big Bool
  | Small Bool
  deriving (Eq, Show)

-- has cardinality 4

data NumberOrBool = 
    Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

-- has cardinality 256 + 2 = 258
myNumba = Numba (-128)