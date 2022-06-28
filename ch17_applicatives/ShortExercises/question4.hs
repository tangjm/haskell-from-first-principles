module Question4 where 

xs = [1, 2, 3]
ys = [4, 5, 6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys 
-- x = Just 6

y :: Maybe Integer
y = lookup 2 $ zip xs ys
-- y = Just 5

summed :: Maybe Integer 
summed = fmap sum $ (,) <$> x <*> y

-- (,) <$> x <*> y = Just (6, 5)
-- fmap sum $ Just (6, 5) = Just 5
