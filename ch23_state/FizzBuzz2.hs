module FizzBuzz2 where

import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Fizz"
           | n `mod` 3  == 0 = "Buzz"
           | otherwise       = show n 

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = 
  execState (mapM_ addResult list) []

{- 
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()

execState expects an entity of type (State s a) and returns the state after discarding the non-state value in the returned tuple.
-}

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get 
  let result = fizzBuzz n 
  put (result : xs)

{- 
xs :: [String]
'put' puts the list of strings inside a monad, producing a State [String] () 
-}

main :: IO ()
main = 
  mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]

{-
mapM_ produces a foldable list of type [IO ()]
Then we fold that list into a single 'IO ()' which is the
return type of our 'main' function
-}
