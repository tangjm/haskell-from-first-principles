module MatchTypes where
import Data.List (sort)


mySort :: [Char] -> [Char]
mySort = sort 

signifier :: [Char] -> Char
signifier xs = head (mySort xs)


