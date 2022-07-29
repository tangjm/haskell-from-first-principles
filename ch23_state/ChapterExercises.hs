{-# LANGUAGE InstanceSigs #-}

module ChapterExercises where

newtype State s a =
  State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State g) = State $ \x -> (f (fst (g x)), x) 
  -- fmap f (State g) = State $ f . fst . g
  
instance Applicative (State s) where
  pure x = State $ \y -> (x, y)
  
  (<*>) :: State s (a -> b) -> State s a -> State s b 
  State f <*> State g = 
   State $ \x -> ((fst (f x)) (fst (g x)), x)

{-
   s -> (a -> b, s)
   s -> (a, s)
   s -> (b, s)
-}

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State f >>= g = 
    -- State $ \x -> ((runState (fst ((runState (g <$> (State f))) x))) x) 
    State $ \x -> (runState (fst ((runState (g <$> (State f))) x))) (snd ((runState (g <$> (State f))) x)) -

{-
State $ s -> (a, s)
a -> State $ s -> (s, b) 
State $ s -> (b, s)

1. State s a
2. State s (State s b)
3. State s b

How we go from 1 and 2 to 3?

g <$> (State f) :: State s (State s b)
runState $ g <$> (State f) :: s -> (State s b, s)
                           :: s -> (s -> (b, s), s)

\x -> (runState (g <$> (State f))) x  :: s -> (State s b, s) 
\x -> ((runState (fst ((runState (g <$> (State f))) x))) x) 
:: State s b

\x -> (runState (fst ((runState (g <$> (State f))) x))) (snd ((runState (g <$> (State f))) x)) 

Applying this final function involves the following reduction:
s -> (State s b, s)
fst (State s b, s)
runState $ State s b 
s -> (b, s)
-}



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