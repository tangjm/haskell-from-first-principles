module QuickCheckTests where

import Test.QuickCheck.Checkers

type TI = []

main = do
  let trigger = undefined :: TI (Int, Int [Int])
  quickBatch (traversable trigger)

