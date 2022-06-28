module ShortExercises where

import Data.List (elemIndex)

-- 1
added :: Maybe Integer
added = fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z 

-- Just 6 -> Just 5 -> Just (6, 5)
-- Just (6,) <*> Just 5 = Just (6, 5)
