module AvgGrade where

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.7 = 'C'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100


numbers x
    | x < 0  = -1
    | x == 0 = 0
    | x > 0  = 1

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y b 
    | _ _ b == True  = x
    | _ _ b == False = y
    