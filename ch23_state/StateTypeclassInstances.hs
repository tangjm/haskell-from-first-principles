{-# LANGUAGE InstanceSigs #-}

module StateTypeclassInstances where

newtype State' s a = 
  State' { runState' :: s -> (a, s) }

instance Functor (State' s) where 
  fmap f (State' g) = 
    State' $ \x -> (f (fst (g x)), x) 

  -- runState' ((+1) <$> (State' $ \s -> (0, s))) 0 


instance Applicative (State' s) where
  pure x = State' $ \y -> (x, y)  

  (<*>) :: State' s (a -> b) -> State' s a -> State' s b
  State' f <*> State' g = 
    State' $ \x -> ((fst (f x)) (fst (g x)), x) 

-- We apply the first element of the tuple returned from f to the first element of the tuple returned from g.

instance Monad (State' s) where
  return = pure
  (>>=) :: State' s a -> (a -> State' s b) -> State' s b
  State' f >>= g = 
    State' $ \x -> runState' (g (fst (f x))) x 

  -- State' s (State' s b)

{- 
  We apply g to the first element returned by f. 
  Then we unwrap the function :: s -> (b, s) from what's returned from g.
  Finally, we apply the unwrapped function to x to produce something of type (b, x) before wrapping that back into the State' type.
-} 