module BinaryTreeFolds where

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- foldr for binary trees using preorder traversal
foldTreePre :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTreePre f acc Leaf = acc
foldTreePre f acc (Node left a right) = f a (foldTreePre f (foldTreePre f acc right) left)

foldTreePost :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTreePost f acc Leaf = acc
foldTreePost f acc (Node left a right) = foldTreePost f (foldTreePost f (f a acc) right) left

foldTreeIn :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTreeIn f acc Leaf = acc
foldTreeIn f acc (Node left a right) = foldTreeIn f (f a (foldTreeIn f acc right)) left

mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' f bt = foldTreePre (\x acc -> case x of 
                                          Leaf -> acc
                                          Node left a right -> Node left (f a) right) Leaf bt

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 2 Leaf) 1 (Node Leaf 3 Leaf)

x = foldTreeIn (+) 0 testTree
y = foldTreePre (+) 0 testTree
z = foldTreePost (+) 0 testTree

inorder = foldTreeIn (:) [] testTree
preorder = foldTreePre (:) [] testTree
postorder = foldTreePost (:) [] testTree

