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
  -- mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]
  mapM_ putStrLn $ fizzbuzzFromTo 1 100 

{-
mapM_ produces a foldable list of type [IO ()]
Then we fold that list into a single 'IO ()' which is the
return type of our 'main' function
-}


-- enumerate our sequence backwards to avoid having to run a costly reverse operation of our resulting singly linked-list.
-- Here n < m where n denotes the lower bound and m denotes the upper bound

-- The following not work because Haskell implicitly assumes that [m..n] will be in ascending order. It's not that there is no guarantee that m > n for all m and n. 

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo n m =  
  execState (mapM_ addResult [m..n]) []

-- We can specify [m, m-1 .. n] to explicity tell the compiler that this will be a descending list.

fizzbuzzFromTo' :: Integer -> Integer -> [String]
fizzbuzzFromTo' n m =
  execState (mapM_ addResult [m, m - 1..n]) [] 
  