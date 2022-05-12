module DividedBy where

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d  = (count, n)
          | otherwise = go (n - 1) d (count + 1)