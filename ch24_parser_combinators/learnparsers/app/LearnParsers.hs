module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser Char 
one' = one >> stop 

-- In the above, while the computed value returned from 'one' is thrown away, the effect of the monadic action remains.
-- So it is some kind of state with possible failure.

{-
char :: CharParsing m => Char -> m Char
unexpected :: Parsing m => String -> m a
-}

oneTwo :: Parser Char  
oneTwo = char '1' >> char '2'

-- oneTwoRetained :: Parser String 
-- oneTwoRetained = char '1' >>= retain 

-- retain :: Char -> Parser String 
-- retain c = string [c, '2', '3']  

oneTwo' :: Parser Char 
oneTwo' = oneTwo >> stop

oneTwoEOF :: Parser () 
oneTwoEOF = oneTwo >> eof 

stringParser :: String -> Parser String
stringParser xs = string xs 

{- 
Write stringParser using the 'char' parser 

At each step, 
  continue parsing if successful
  return Failure if unsucessful
At the end, return the successfully parsed result if no failure has yet occured.
-}
charParser :: Parser Char 
           -> Parser Char
           -> Parser Char 
           -> String 
           -> Result String 
charParser p1 p2 p3 s = 
  parseString p1 mempty s 
   >>= \x -> parseString p2 mempty (tail s) 
     >>= \y -> parseString p3 mempty (tail . tail $ s) 
      >>= \z -> Success [x, y, z]

testParse :: Parser Char -> IO ()
testParse p = 
  print $ parseString p mempty "123"

testParseString :: Parser String -> IO ()
testParseString p = 
  print $ parseString p mempty "123"

testParsePolymorphic :: Show a => Parser a -> IO ()
testParsePolymorphic p = 
  print $ parseString p mempty "123"

-- We're combining parsers using functions from the Applicative and Monad typeclasses like (>>).

{-
parseString :: Parser a -> 
               Text.Trifecta.Delta.Delta ->
               String ->
               Result a
               
Text.Trifecta.Delta.Delta keeps track of the cursor position.

string :: String -> Parser String
eof :: Parser ()
-}

pNL s = putStrLn ('\n' : s)

main = do 
  pNL "stop:"
  testParse stop 
  pNL "one:"
  testParse one 
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo 
  pNL "oneTwo':"
  testParse oneTwo'

  pNL "oneString:"
  testParseString $ stringParser "1"
  pNL "oneTwoString:"
  testParseString $ stringParser "12"
  pNL "oneTwoThreeString:"
  testParseString $ stringParser "123"
  pNL "oneTwo then stop:"
  testParseString $ stringParser "12" >> stop

  pNL "charParser:"
  print $ charParser (char '1') (char '2') (char '3') "123"
