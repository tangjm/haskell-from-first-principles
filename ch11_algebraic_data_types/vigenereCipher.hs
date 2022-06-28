module VigenereCipher where

import Data.Char

-- Helper functions

encodingKey :: String
encodingKey = "caesar"

-- Suppose encodingKey = "dog" for our examples
-- Map indexes of letters of our message to indexes of our encodingKey
-- "jared" -> [0, 1, 2, 0, 1]
toEncodingWordIndex :: String -> [Int]
toEncodingWordIndex xs = map (flip mod $ length encodingKey) [0..length xs - 1]

-- Map indexes to letters of our encodingKey
-- [0, 1, 2, 0, 1] -> ['d', 'o', 'g', 'd', 'o']
indexToEncodingLetter :: [Int] -> [Char]
indexToEncodingLetter = map (\x -> encodingKey !! x)

-- Map letters of our encodingKey to the number of right shifts that they represent
-- "dogdo" -> [3, 14, 6, 3, 14]
letterToShiftCount :: [Char] -> [Int]
letterToShiftCount = map (flip (-) 97 . ord)

-- Composition of letterToShiftCount, indexToEncodingLetter and toEncodingWordIndex
determineShiftAmount :: String -> String -> [Int]
determineShiftAmount key xs = z
  where x = map (flip mod $ length key) [0..length xs - 1]
        y = map (\x -> key !! x) x
        z = map (flip (-) 97 . ord) y

-- To go from unicode to letters, we can look through the alphabet and the letter s.t. (flip mod 26 . ord) letter == 'our unicode' is true, will  be the corresponding letter.

-- Returns the letter of the alphabet that matches our argument
equalsX :: Int -> Char
equalsX x = head $ filter (((==) . flip mod 26 $ x) . flip mod 26 . ord) ['a'..'z'] 

-- ENCODING

-- Takes an encoding word and a message to encode and returns the result of encoding the message with the encoding word using a vigenere cipher
encode' :: String -> String
encode' msg = zipWith (\shiftCount letter -> equalsX $ ord letter + shiftCount) shiftAmount msg
  where shiftAmount = 
          letterToShiftCount . indexToEncodingLetter . toEncodingWordIndex $ msg

-- Rather than hard coding our encoding key, we specify it as an argument
encode :: String -> String -> String
encode key msg = zipWith (\shiftCount letter -> equalsX $ ord letter + shiftCount) shiftAmount msg
  where shiftAmount = determineShiftAmount key msg



-- DECODING
-- Same as 'encode' but shifts in the opposite direction.
decode :: String -> String -> String
decode key msg = zipWith (\shiftCount letter -> equalsX $ ord letter - shiftCount) shiftAmount msg
  where shiftAmount = determineShiftAmount key msg

-- SENTENCES

-- Encoding a sentence
-- split sentence into a list by whitespace and then apply the 'encode' function to each element
-- then flatten the list of strings back into an encoded string   
-- String -> [String] -> String

encodeSent :: String -> String -> String
encodeSent = 