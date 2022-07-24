module RandomExample where

import System.Random

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State

data Die = 
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving(Eq, Show)

intToDie :: Int -> Die 
intToDie n =
  case n of 
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- This catch-all pattern is used here, but better practice would be to use the Maybe or Either data types instead.
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

-- Now making use of the State newtype

rollDie :: State StdGen Die 
rollDie =
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie 

main :: IO ()
main = do
  print $ evalState rollDie (mkStdGen 0)
  print $ evalState rollDie (mkStdGen 1)
  print $ evalState rollDieThreeTimes' (mkStdGen 0)
  print $ evalState rollDieThreeTimes' (mkStdGen 1)
  print $ evalState (nDie 2) (mkStdGen 0)
  print $ evalState (nDie 2) (mkStdGen 1)
  print $ evalState (nDie 5) (mkStdGen 0)
  print $ evalState (nDie 5) (mkStdGen 1)

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- this repeats the die value in 'rollDie :: State StdGen Die' similar to how 'repeat 1' will produce a list of 1's. 

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie 

-- By contrast, this repeats the 'rollDie' computation.

rollsToGetTwenty :: StdGen -> Int 
rollsToGetTwenty g = go 0 0 g 
  where go :: Int -> Int -> StdGen -> Int 
        go sum count gen 
          | sum >= 20 = count 
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen 
            in go (sum + die) (count + 1) nextGen 

-- returns the number of dice rolls needed to roll a cumulative total of at least 20

-- automated = (rollsToGetTwenty . mkStdGen) <$> randomIO 

rollsToGetN :: Int -> StdGen -> Int 
rollsToGetN n g = go 0 0 g 
  where go :: Int -> Int -> StdGen -> Int  
        go sum count gen 
          | sum >= n = count 
          | otherwise = 
            let (die, nextGen) = randomR (1, 6) gen 
            in go (sum + die) (count + 1) nextGen 

-- returns the number of dice rolls needed to roll a cumulative total of at least N 

rollsCountLogged :: Int -> StdGen -> (Int, [Die]) 
rollsCountLogged n g = go 0 0 [] g 
  where go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
        go sum count history gen 
          | sum >= n = (count, history) 
          | otherwise = 
            let (die, nextGen) = randomR (1, 6) gen 
            in go (sum + die) (count + 1) (history ++ [intToDie die]) nextGen 


-- returns the number of die rolls needed to rolls a cumulative total of at least N in addition to a history of those dice rolls. 