module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: CharParsing m => m Char
one = char '1'

one' = one >> stop 

-- In the above, while the computed value returned from 'one' is thrown away, the effect of the monadic action remains.
-- So it is some kind of state with possible failure.

{-
char :: CharParsing m => Char -> m Char
unexpected :: Parsing m => String -> m a
-}
