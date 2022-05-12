module ZipMyVersion where 

 
myZip :: [a] -> [b] -> [(a, b)]
-- zip = undefined
-- myZip xs ys = go xs ys []
--   where go xs ys tupleList
--           | length xs == 0 || length ys == 0 = tupleList
--           | otherwise                        = go (tail xs) (tail ys) (tupleList ++ [(head xs, head ys)])

myZip [] _ = []
myZip _ [] = []
myZip xs ys = (head xs, head ys) : myZip (tail xs) (tail ys)


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f xs ys = f (head xs) (head ys) : myZipWith f (tail xs) (tail ys)


myZipFromZipWith :: [a] -> [b] -> [(a, b)]
myZipFromZipWith = myZipWith (,)



