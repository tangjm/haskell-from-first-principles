module BinaryTree where

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

foldTreePre :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTreePre f acc Leaf = acc
foldTreePre f acc (Node left a right) = f a (foldTreePre f (foldTreePre f acc right) left)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b 
mapTree f Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b 
mapTree f Leaf = foldTreePre f' acc Leaf = acc
mapTree f (Node left a right) = foldTree f' acc (Node left a right) 
                              = f' a (foldTreePre f' (foldTreePre f' acc right) left)
                              = f' a (foldTreePre f' acc left)
                              = f' a acc

mapTree f bt = foldTreePre (\x acc -> Node ) Leaf bt
foldTreePre f acc Leaf = Leaf 
foldTreePre f acc (Node Leaf 1 Leaf) = f 1 (foldTreePre f (foldTreePre f acc Leaf) Leaf)
                                     = f 1 (foldTreePre f (foldTreePre f Leaf Leaf) Leaf)
                                     = f 1 (foldTreePre f (Leaf) Leaf)
                                     = f 1 Leaf
                                     = Node Leaf (f 1) Leaf

foldTreePre f acc (Node (Node Leaf 1 Leaf) 2 Leaf) = ???
foldTreePre f acc (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) = ???
                        


testTree' :: BinaryTree Integer 
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay = 
  if mapTree (+1) testTree' == mapExpected
  then print "yep okay!"
  else error "test failed!"




