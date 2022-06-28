module AsPatterns where

import Data.Char

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x : _) = x : xs

-- Returns True if all elements of the first list are members of the second 
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] ys = True
isSubsequenceOf xs [] = False
isSubsequenceOf (x : xs) list2@ys = if any (==x) list2 then isSubsequenceOf xs list2 else False

-- There's probably a better way to implement this



type Delimiter = Char

-- Splits a string into a list of strings based on a user specified character
splitString :: Delimiter -> String -> [String]
splitString d [] = []
splitString d xs = ([] ++ [takeWhile (/=d) xs]) ++ splitString d (dropWhile (==d) . dropWhile (/=d) $ xs)

-- Splits a sentence into words by whitespace characters
toWords :: String -> [String]
toWords = splitString (' ')

-- Splits a sentence into words and turns each word and its capitalised form into an ordered pair 
capitaliseWords :: String -> [(String, String)]
capitaliseWords xs = zip (toWords xs) xs'
  -- where xs' = map (\x -> toUpper (head x) : tail x) (toWords xs)
  where xs' = map (\str@(y:ys) -> toUpper y : tail str) (toWords xs)


