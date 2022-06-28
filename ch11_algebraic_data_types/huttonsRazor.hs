module HuttonsRazor where

import Data.Char

data Expr = Lit Integer
          | Add Expr Expr

eval :: Expr -> Integer 
eval (Lit n) = n 
eval (Add n m) = eval n + eval m

-- e.g. Add (Add Lit 2 Lit 3) (Add Lit 4 Lit 5)


-- Part 2. Write a function to print an Expr.
-- E.g. printExpr (Add (Add (Lit 2) (Lit 3)) (Add (Lit 4) (Lit 5))) should give "2 + 3 + 4 + 5"

-- In order to do this, we need to cast an Integer to a String. E.g. 2 -> '2' and 9001 -> '9001'

-- We need a recursive function to get the digits from a number.
-- For any integer n, we map n -> [d0, d1 .. dn], where 'd0' to 'dn' are the digits of n.

-- Then we map digits to their unicode values as follows: [0..9] -> [48..57] 

-- Then we map unicode values to a list of their character representations, namely, [u0..un] -> [chr u0 .. chr un]

-- [chr x | x <- [48..57]] gives us the list ['0'..'9']

type Digit = Integer

-- This gets us the length of an integer
getLength :: Integer -> Digit
getLength x = go x 1
  where go x count
          | x `div` 10 == 0 = count
          | otherwise       = go (x `div` 10) (count + 1)

-- Plan for getDigits function 

-- Step 1
-- Call (div 10) recursively.
-- If the recursive call evaluates to 0, then stop making further recursive calls.

-- Step 2
-- Then evaluate the expression replacing (div 10) with (mod 10) at each recursive step and add the intermediate results to a list going from right to left.

-- Alternatively, we keep track of the number of recursive calls before our expression evalutes to 0.
-- Then make use of this to apply (mod 10 . k) function, where k denotes the number of composed (div 10) functions, as many times as needed until k equals the number of recursive (div 10) calls before our expression evaluates to 0, starting with k = 0.

getDigits :: Integer -> [Digit]
getDigits x = go x []
  where go x acc
          | x `div` 10 == 0 = x `mod` 10 : acc
          | otherwise       = go (x `div` 10) (x `mod` 10 : acc)

-- It just so happens that each element corresponds to its index in [0..9].
-- So, to map digits to their unicode representations, we can map [0..9] to [48..57] by simply treating our digits as indexes. For example, 0 will be [48..57] !! 0. And 9 will be [48..57] !! 9.

-- We also need to cast Integer to Int because our 'chr' function only works with Ints.
-- 1. We can use fromInteger to take us from an Integer to Num.
-- 2. Then we can use chr with Int because an Int has an instance of the Num typeclass 

digitsToString :: [Digit] -> [Char]
digitsToString xs = [ chr $ [48..57] !! fromInteger x | x <- xs ]


printExpr :: Expr -> String
printExpr (Lit n) = digitsToString . getDigits $ n
printExpr (Add n m) = printExpr n ++ " + " ++ printExpr m