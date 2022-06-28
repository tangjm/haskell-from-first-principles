module WriteFunctorInstances where

-- 1
data Quant a b =
    Finance   
  | Desk a  
  | Bloor b

instance Functor (Quant x) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b) 

-- 2
data K a b = K a 

instance Functor (K a) where
  fmap _ (K a) = K a  





