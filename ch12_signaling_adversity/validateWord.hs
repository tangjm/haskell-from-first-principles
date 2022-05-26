module ValidateWord where


newtype Word' = 
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

-- Fold over the elements of our string, checking if they are vowels.
-- We keep count of the number of vowels.
-- The consonant count will be the result of subtracting the number of vowels from the length of our string.
-- Finally, we check if COUNT(v) > COUNT(c) and return True if so and False if not.

-- Nifty solution
-- Increment our accumulator for each vowel; decrement our accumulator for each consonant.
-- If the final accumulator has value > 0, then there are more vowels than consonants.
-- Otherwise, there are either as many vowels as consonants or less vowels than consonants.

countVowels :: String -> Integer
countVowels = foldr (\x y -> if x `elem` vowels then y + 1 else y - 1) 0 


-- Returns Nothing if the number of vowels > numbers of consonants
-- Else returns Just Word'
mkWord :: String -> Maybe Word'
mkWord xs = if countVowels xs > 0 then Nothing else Just (Word' xs)
