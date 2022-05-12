module Reverse where

rvrs :: String -> String
rvrs s = z ++ y ++ x
  where x = take 5 s
        y = take 4 (drop 5 s)
        z = drop 9 s


main :: IO ()
main = print $ rvrs "Curry is awesome"