module ReaderPractice where

import Control.Applicative 
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]


-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer 
zs = lookup 4 $ zip x y 

z' :: Integer -> Maybe Integer
z' n =  lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (x, x) where x = z' n 

summed :: Num c => (c, c) -> c 
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)
-- n.b. this is an example of a Reader

fromMaybe :: a -> Maybe a -> a 
fromMaybe x Nothing = x
fromMaybe x (Just y) = y 

main :: IO ()
main = do 
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7 
  print $ fmap bolt z 
  print $ sequenceA [(>3), (<8), even] 7
  print $ sequA $ ReaderPractice.fromMaybe 4 s'
  print $ bolt $ ReaderPractice.fromMaybe 4 ys
  print $ bolt $ ReaderPractice.fromMaybe 4 (z' 3)
    
sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m 

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

foldBoolConj :: Bool
foldBoolConj = foldr (&&) True (sequA 4)
