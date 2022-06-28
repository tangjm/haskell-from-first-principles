module Question10 where 

data GoatLord a = 
    NoGoat 
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where 
  fmap f NoGoat = NoGoat 
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats left middle right) = MoreGoats (fmap f left) (fmap f middle) (fmap f right)