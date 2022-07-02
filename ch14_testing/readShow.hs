module ReadShow where

import Test.QuickCheck

-- show :: Show a => a -> String
-- read :: Read a => String -> a 

f :: (Eq a, Show a, Read a) => a -> Bool
f x = (read (show x)) == x

f' :: Int -> Bool
f' = f

main :: IO ()
main = do
  quickCheck f'