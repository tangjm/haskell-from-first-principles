module MyWords where

myWords :: [Char] -> [[Char]]
-- myWords s = go s []
--   where go s newList
--           | takeWhile (/= ' ') s == [] = newList
--           | takeWhile (/= ' ') s == s  = newList ++ [s]
--           | otherwise                  = go (dropWhile (== ' ') $ dropWhile (/= ' ') s) (newList ++ [takeWhile (/= ' ') s])

myWords s = go s []
  where go s newList
          | takeWhile f s == [] = newList
          | takeWhile f s == s  = newList ++ [s]
          | otherwise           = go (dropWhile f' . dropWhile f $ s) (newList ++ [takeWhile f s])
          where f  = (/= ' ')
                f' = (== ' ')


splitSentenceBy :: [Char] -> Char -> [[Char]]
splitSentenceBy s c = go s []
  where go s charList
          | takeWhile f s == s = charList ++ [s]
          | otherwise          = go (dropWhile f' . dropWhile f $ s) (charList ++ [takeWhile f s])
          where f = (/= c)
                f' = (== c)

myWordsGeneric :: [Char] -> [[Char]]
myWordsGeneric = flip splitSentenceBy ' '