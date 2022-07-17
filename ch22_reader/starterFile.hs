module StarterFile where

import Control.Applicative

hurr :: Num a => a -> a
hurr = (*2) 

durr :: Num a => a -> a
durr = (+10)

m :: Num a => a -> a
m = hurr . durr 

m' :: Integer -> Integer 
m' = fmap hurr durr 

{-

n.b. fmap composes the two functions before applying them

m' x == (*2) ((+10) x)

-}

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr 

-- Here we are applying a binary function to two partially applied functions
