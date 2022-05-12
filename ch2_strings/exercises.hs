module Exercises where

exclaim :: String -> String
exclaim s = s ++ "!"

returnY :: String -> String
returnY s = "y"

shift9 :: String -> String
shift9 s = drop 9 s

thirdLetter :: String -> Char
-- thirdLetter s = head (drop 2 s)
thirdLetter s = s !! 2

letterIndex :: Int -> Char
letterIndex x = s !! x
  where s = "Curry is awesome!"

rvrs :: String -> String
rvrs s = z ++ y ++ x
  where x = take 5 s
        y = take 4 (drop 5 s)
        z = drop 9 s
