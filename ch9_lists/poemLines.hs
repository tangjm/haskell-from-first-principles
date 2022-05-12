module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

splitSentenceBy :: [Char] -> Char -> [[Char]]
splitSentenceBy s c = go s []
  where go s charList
          | takeWhile f s == s = charList ++ [s]
          | otherwise          = go (dropWhile f' . dropWhile f $ s) (charList ++ [takeWhile f s])
          where f = (/= c)
                f' = (== c)

myLines :: String -> [String]
myLines poem = go poem []
  where go poem lines 
          | takeWhile f poem == []   = lines
          | takeWhile f poem == poem = lines ++ [poem]
          | otherwise                = go (dropWhile f' . dropWhile f $ poem) (lines ++ [takeWhile f poem])
          where f = (/= '\n')
                f' = (== '\n')

myLinesGeneric :: String -> [String]
myLinesGeneric = flip splitSentenceBy '\n'

-- What we want 'myLines sentences' to equal
shouldEqual = [ "Tyger Tyger, burning bright"
              , "In the forests of the night"
              , "What immortal hand or eye"
              , "Could frame thy fearful symmetry?"
              ]
-- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main = 
  print $ "Are they equal? " 
          ++ show (myLinesGeneric sentences == shouldEqual)