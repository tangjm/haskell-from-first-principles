module EitherLibrary where

-- produces a list comprising only left values
lefts' :: [Either a b] -> [a]
lefts' = foldr (\x y -> case x of 
                          Left x'  -> x' : y
                          Right x' -> y) [] 
 
rights' :: [Either a b] -> [b]
rights' = foldr (\x y -> case x of 
                          Left x'  -> y
                          Right x' -> x' : y) [] 

partitionEithers' :: [Either a b] -> ([a], [b])
-- partitionEithers' xs = (lefts' xs, rights' xs)
partitionEithers' = foldr (\x y -> case x of 
                                    Left x'  -> (x' : fst y, snd y)
                                    Right x' -> (fst y, x' : snd y)) ([],[])

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c 
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left a) = f a
either' f g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (\_ -> Nothing) (Just . f) x