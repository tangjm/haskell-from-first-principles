module StringProcessing where

notThe :: String -> Maybe String
notThe xs = if xs == "the" then Nothing else Just xs

-- we create a list of strings and then fold over that list

-- splits a string into a list of strings on ' '
-- my own implementation of 'words'
splitByWord :: String -> [String]
splitByWord [] = []
splitByWord (' ' : xs) = splitByWord xs
splitByWord xs = takeWhile (/=' ') xs : splitByWord (dropWhile (/=' ') xs)


replaceThe :: String -> String
replaceThe xs = foldr (\x y -> case notThe x of 
                                Nothing -> "a" ++ z
                                  where z | y == []   = y
                                          | otherwise = " " ++ y
                                Just x -> x ++ z
                                  where z | y == []   = y
                                          | otherwise = " " ++ y) [] (splitByWord xs)

-- countTheBeforeVowel :: String -> Integer
-- countTheBeforeVowel xs = go xs 0
--   where go xs count
--           | xs == [] = count
--           | (notThe . head) xs == Nothing && (head . head . tail) xs `elem` ['a', 'e', 'i', 'o', 'u'] = go (tail xs) (count + 1)
--           | otherwise = go (tail xs) count


f :: [String] -> (String, Integer)
f = foldr (\x (ys, count) -> case notThe x == Nothing && head ys `elem` "aeiou" of 
                              True -> (x ++ ys, count + 1)
                              False -> (x ++ ys, count)) ([], 0) 

-- Counts the number of 'the' words succeeded by a word beginning with a vowel.
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = snd . f . splitByWord 

-- Counts the number of vowels in a string
countVowels :: String -> Integer
countVowels = foldr (\x y -> if x `elem` "aeiou" then y + 1 else y) 0 

countVowels' :: String -> Int
countVowels' xs = length $ filter (flip elem "aeiou") xs
