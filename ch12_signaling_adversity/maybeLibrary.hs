module MaybeLibrary where

isJust :: Maybe a -> Bool   
isJust (Just x) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool  
isNothing (Just x) = False
isNothing Nothing = True

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee acc f Nothing = acc
mayybee acc f (Just x) = f x

-- is this correct?

fromMaybe :: a -> Maybe a -> a
-- fromMaybe y Nothing = y
-- fromMaybe y (Just x) = x

-- now written in terms of mayybee
fromMaybe y x = mayybee y id x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- drop Nothing values from a list of Maybe a
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x y -> case x of  
                            Nothing -> y
                            Just x' -> x' : y) []

-- If all elements of our list is constructed using the (Just a) data constructor, move the data constructor to the front.
-- Otherwise, return Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if all isJust xs
               then Just $ map (\(Just x) -> x) xs
               else Nothing

