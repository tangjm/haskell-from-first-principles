module MultRecursive where

mult :: Integral a => a -> a -> a
mult 0 m = 0
mult n m = mult (n - 1) m + m