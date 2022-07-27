module FizzBuzz where

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Fizz"
           | n `mod` 3  == 0 = "Buzz"
           | otherwise       = show n 

main :: IO ()
main = 
  mapM_ (putStrLn . fizzBuzz) [1..100]

{- 

mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()

This is a foldMap for Monads that maps each Integer to an IO () and then folds the list of IO () to produce a single IO ().

-}