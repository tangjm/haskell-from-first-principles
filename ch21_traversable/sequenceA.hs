module SequenceA where

fmap :: (a -> b) -> f a -> f b 
(.) :: (a -> b) -> (b -> c) -> a -> c

fmap . fmap :: (a -> b) -> 

(fmap . fmap) sum Just [1, 2, 3]
