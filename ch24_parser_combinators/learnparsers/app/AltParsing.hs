{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ

type NumberOrString =
  Either Integer String 

eitherOr :: String 
eitherOr = [r|
123
abc
456
def
|]

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = 
      (Left <$> integer)
  <|> (Right <$> some letter)

-- (<|>) :: Applicative f => f a -> f a -> f a 

parseNos' :: Parser NumberOrString
parseNos' = 
  skipMany (oneOf "\n")
  >>
      (Left <$> integer)
  <|> (Right <$> some letter)


main = do
  print $ parseString (some letter) mempty a
  print $ parseString integer mempty b 
  print $ parseString parseNos mempty a 
  print $ parseString parseNos mempty b 
  print $ parseString (many parseNos) mempty c 
  print $ parseString (some parseNos) mempty c

  print $ parseString parseNos mempty eitherOr
  -- This fails as 'eitherOr' begins with a newline character '\n' which is neither an integer nor a string. 

  print $ parseString parseNos' mempty eitherOr 

  print $ parseString (some parseNos') mempty eitherOr 
  -- This one fails because of the the trailing "\n" after 'def' in eitherOr. parseNos' requires that there be either an integer or some character after skipping zero or more newline characters.

  print $ parseString (some $ token parseNos') mempty eitherOr
  -- With 'token', we sequence parseNos' with 'whiteSpace' which skips zero or more bytes of whitespace and then return what's parsed by parseNos'
  -- token :: TokenParsing m => m a -> m a
  -- whiteSpace :: TokenParsing m => m ()