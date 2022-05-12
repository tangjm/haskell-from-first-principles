module Fibonacci where

fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib x = fib (x - 1) + fib (x - 2)

fibsInfinite :: [Int]
fibsInfinite = 1 : scanl (+) 1 fibsInfinite

fibsN :: Int -> Int
fibsN = (!!) fibsInfinite


firstTenFibNum :: [Int]
firstTenFibNum = take 10 fibsInfinite

firstTwentyFibs :: [Int]
firstTwentyFibs = take 20 fibsInfinite

fibsLessThan100 :: [Int]
fibsLessThan100 = takeWhile (<100) fibsInfinite 

fibsLessThan400:: [Int]
fibsLessThan400 = takeWhile (<400) fibsInfinite