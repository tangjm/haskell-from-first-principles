module RandomGenerators where

import System.Random

{- 
mkStdGen :: Int -> StdGen
StdGen is a pair of Int32 values, e.g. (2, 4)

next :: g -> (Int, g) 
g has type StdGen

random :: (RandomGen g, Random a) => g -> (a, g)
let's us generate random values that aren't numbers.

randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
let's us generate a random number within a range (i, j)
-}

sg :: StdGen
sg = mkStdGen 0

sg2 :: StdGen
sg2 = mkStdGen 1

newSg = snd (next sg)
first = next newSg 

-- random :: Random a => (a, StdGen)
-- random newSg

main :: IO ()
main = do
  print $ mkStdGen 0 