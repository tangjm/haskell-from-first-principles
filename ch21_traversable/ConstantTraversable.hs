module ConstantTraversable where

newtype Constant a b = 
  Constant { getConstant :: a }


instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ (Constant x) = mempty 

instance Traversable (Constant a) where
  traverse f (Constant x) = pure (Constant x)