module ExerciseP857 where

import Text.Trifecta 

-- Write a function that parses only pure integers and fails otherwise.

myParser :: Parser Integer 
myParser = do 
  parsedInt <- integer
  eof 
  return parsedInt

myParser' :: Parser Integer 
myParser' = 
  integer >>= 
    \x -> eof >>=
      \_ -> return x

myParser'' :: Parser Integer 
myParser'' = 
  integer >>= 
   \x -> notFollowedBy anyChar >>=
    \_ -> return x

-- another variant making use of 'notFollowedBy'

main :: IO ()
main = do 
  putStrLn "myParser test:"
  print $ parseString myParser mempty "123"
  print $ parseString myParser mempty "123abc"

  putStrLn "\nmyParser' test:"
  print $ parseString myParser' mempty "123"
  print $ parseString myParser' mempty "123abc"

  putStrLn "\nmyParser'' test:"
  print $ parseString myParser'' mempty "123"
  print $ parseString myParser'' mempty "123abc"