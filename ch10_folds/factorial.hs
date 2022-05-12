module Factorial where

fact :: Integral a => [a]
fact = 1 : scanl (\x y -> x * ((+1) $ div x y)) 1 fact

factN :: Integral a => Int -> a
factN = (!!) fact

firstTwentyFacts :: Integral a => [a]
firstTwentyFacts = take 20 fact

factsLessThan100 :: Integral a => [a]
factsLessThan100 = takeWhile (<100) fact


secretFunc :: Fractional a => String -> a
secretFunc x =
  (/) (fromRational . toRational $ (sum (map length (words x))))
      (fromRational . toRational $ (length (words x)))