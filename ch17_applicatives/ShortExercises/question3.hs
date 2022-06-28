module Question3 where 
import Data.List (elemIndex)

-- 3
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5] 

max' :: Int -> Int -> Int 
max' = max 

maxed :: Maybe Int 
maxed = fmap max' x <*> y

-- x = Just 2 
-- y = Just 3
-- fmap max' x = Just (max 2)
-- Just (max 2) <*> Just 3 = Just 3