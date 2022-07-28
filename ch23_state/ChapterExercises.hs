{-# LANGUAGE InstanceSigs #-}

module ChapterExercises where

newtype State s a =
  State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State g) = State $ \x -> (f (fst (g x)), x) 
  -- fmap f (State g) = State $ f . fst . g
  
-- instance Applicative (State s) where
--   pure x = State $ \y -> (x, y)
--   (<*>) :: State s (a -> b) -> State s a -> State s b 
--   State f <*> State g = 
--    State $ \x -> f (fst (g x)) 

-- instance Monad (State s) where
--   State f >>= g = 
--    State $ g . f 

get :: State s s
get = State $ \x -> (x, x)

put :: s -> State s () 
put s = State $ \x -> ((), s)

-- The default value is unit or '()' and we ignore the state, instead returning the argument to 'put' as our state. 

exec :: State s a -> s -> s 
exec (State sa) = snd . runState (State sa) 

{-
Some examples 

exec (put "Kant") "Plato"
exec get "Descartes"
-}

eval :: State s a -> s -> a 
eval (State sa) = fst . runState (State sa)

modify :: (s -> s) -> State s ()
modify f = State $ \x -> ((), f x)

modify' :: (s -> s) -> State s ()
modify' = \x -> State $ \y -> ((), x y)

{-
Examples
runSTate (modify (+1)) 0
We seem to need a Monad instance for this one to work:
runState (modify (+1) >> modify (+1)) 0 
-}