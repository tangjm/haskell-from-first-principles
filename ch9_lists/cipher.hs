module Cipher where  

import Data.Char 

-- right shift 3
-- "abc" -> "def"
-- unicode 97 to 122
rShiftWith :: Int
rShiftWith = 3

caesarWith :: Int -> String -> String 
caesarWith n = map f
  where f x = encode . flip mod 26 . (+n) . ord . toLower $ x

caesarRS3 :: String -> String
caesarRS3 = caesarWith 3

unCaesarRS3 :: String -> String
unCaesarRS3 = caesarWith 23

encode :: Int -> Char
encode n = go n encoding
  where go n y
          | n == snd (head y) = fst (head y)
          | otherwise         = go n (tail y)
        encoding = [(x, mod (ord x) 26) | x <- ['a'..'z']]
                  
caesar :: String -> String 
caesar = map $ encode . flip mod 26 . (+3) . ord . toLower

unCaesar :: String -> String 
unCaesar = map $ encode . flip mod 26 . (+23) . ord . toLower

main :: IO ()
main = do
        print $ caesar "jared"
        print $ unCaesar "mduhg"
