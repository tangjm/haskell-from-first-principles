module LanguageExercises where

import Data.Char

capitaliseWord :: String -> String
capitaliseWord [] = []
capitaliseWord (x : xs) = toUpper x : xs

splitByParagraph :: String -> [String]
splitByParagraph [] = []
splitByParagraph ('.' : xs) = splitByParagraph xs
splitByParagraph (' ' : xs) = splitByParagraph xs
splitByParagraph xs = takeWhile (/='.') xs : splitByParagraph (dropWhile (/='.') xs)

capitaliseParagraph :: String -> String
-- capitaliseParagraph xs = foldr (\(k : ks) l -> toUpper k : ks ++ l) "" (splitByParagraph xs)
-- capitaliseParagraph xs = foldr (\x y -> case y of 
--                                           ""        -> x ++ "." ++ y
--                                           otherwise -> x ++ "." ++ " " ++ y) "" list
--                             where list = map (\(z : zs) -> toUpper z : zs) (splitByParagraph xs)

capitaliseParagraph xs = foldr (\(z : zs) y -> case y of 
                                                ""        -> toUpper z : zs ++ "." ++ y
                                                otherwise -> toUpper z : zs ++ "." ++ " " ++ y) "" (splitByParagraph xs)
