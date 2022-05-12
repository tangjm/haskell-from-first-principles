module CapitaliseFirstLetter where 
import Data.Char (toUpper)

capitaliseFirst :: String -> String 
capitaliseFirst xs = toUpper (head xs) : tail xs

capitaliseFirstRecursive :: String -> String
capitaliseFirstRecursive [] = []
capitaliseFirstRecursive xs = toUpper (head xs) : capitaliseFirstRecursive (tail xs)


getFirstCapitalised :: String -> Char
getFirstCapitalised = toUpper . head
