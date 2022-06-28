module Question7 where 

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g a)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where 
  fmap _ (IgnoreSomething (f a) (g a)) = IgnoreSomething (f a (g a))