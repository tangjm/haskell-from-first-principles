module ShortExercise where

import Data.Char 

cap :: [Char] -> [Char] 
cap xs = map toUpper xs 

rev :: [Char] -> [Char] 
rev xs = reverse xs 

composed :: [Char] -> [Char]
composed = rev . cap 

fmapped :: [Char] -> [Char]
fmapped = rev <$> cap 

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  x <- rev 
  y <- cap
  return (x, y)


-- tupled'' :: [Char] -> ([Char], [Char])
-- tupled'' =
--   \x -> 
  
-- Implement tupled' using (>>=) instead of 'do'
-- Come back to this later

