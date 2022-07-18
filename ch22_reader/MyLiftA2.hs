module MyLiftA2 where

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c 
myLiftA2 l2 f g = l2 <$> f <*> g
