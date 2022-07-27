module FizzBuzz3 where

import Control.Monad
import Control.Monad.State
import qualified Data.DList as DL 

{-
DList is a difference list which has a more efficient 'append' operation compared to '++' defined for lists. 
Link to documentation: https://hackage.haskell.org/package/dlist-1.0/docs/Data-DList.html#g:1

-}

fizzBuzz :: Integer -> String 
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Fizz"
           | n `mod` 3  == 0 = "Buzz"
           | otherwise       = show n 

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = 
  let dlist = execState (mapM_ addResult list) DL.empty 
  in DL.apply dlist [] 

{- 
We don't necessarily need to convert our DL.DList String to a [String] because DL.DList also has an instance of Foldable. 
So the mapM_ in our 'main' function can take DL.DList String as a second argument.
Hence, we can simplify 'fizzbuzzList' 
-}

fizzbuzzList' :: [Integer] -> DL.DList String
fizzbuzzList list = 
  execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

-- DL.snoc appends 'result' to xs.

main :: IO ()
main = 
  -- mapM_ putStrLn $ fizzbuzzList [1..100]
  mapM_ putStrLn $ fizzbuzzList' [1..100]