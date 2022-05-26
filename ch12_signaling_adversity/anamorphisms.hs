module Anamorphisms where

-- Lists
myIterate :: (a -> a ) -> a -> [a]
myIterate f x = [x] ++ myIterate f (f x)

-- myIterate f x = [x, f x , f f x, f f f x ...]

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
                  Nothing      -> []
                  Just (x, x') -> [x] ++ myUnfoldr f x' 

-- myUnfoldr (\x -> Just (x, x + 1)) 0 = [0..]

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x
-- betterIterate f x = myUnfoldr (Just . (,) x f) x

-- f x = Just (x, f x)

-- Trees
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- unfold for BinaryTree
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of 
              Nothing           -> Leaf
              Just (x', y, x'') -> Node (unfold f x') y (unfold f x'')

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x == n 
                            then Nothing 
                            else Just (x + 1, x, x + 1)) 0