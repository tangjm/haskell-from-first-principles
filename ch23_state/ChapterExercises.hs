{-# LANGUAGE InstanceSigs #-}

module ChapterExercises where

newtype State s a =
  State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State g) = State $ \x -> (f (fst $ g x), (snd $ g x)) 
  -- fmap f (State g) = State $ f . fst . g
  
instance Applicative (State s) where
  pure :: a -> State s a
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
  return = pure 

  (>>=) :: State s a -> (a -> State s b) -> State s b
  State f >>= g = 
    State $ \x -> (runState (fst $ fn x)) (snd $ fn x) 
              where fn = runState (g <$> (State f)) 
{- 
Solved problem where 'runState (modify (+1) >> modify (+1)) 0' returns ((), 1) instead of ((), 2) 

The problem turned out to be with the Functor instance.
Because we had 
    fmap f (State g) = State $ \x -> (f (fst $ g x), x) 
instead of
    fmap f (State g) = State $ \x -> (f (fst $ g x), (snd $ g x)) 

The following part of our implementation of (>>=) would produce the unwanted result

runState $ g <$> (State f) :: s -> (State s b, s)
                           :: s -> (s -> (b, s), s)

What happens is that the argument 's' passed to s -> (b, s) is the same as the 's' passed to s -> (s -> (b, s), s). This isn't what we want as we want the second element of (s -> (b, s), s) to be passed to s -> (b, s).

Given 'modify (+1)' the types would be s -> (s -> (b, s + 1), s + 1)
And so 
runState $ (modify (+1) >> modify (+1)) 0  will produce ((), 1) instead of ((), 2) 

-}

{-
Notes on (>>=) implementation

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

Initial implementation
\x -> (runState (g <$> (State f))) x  :: s -> (State s b, s) 
\x -> ((runState (fst ((runState (g <$> (State f))) x))) x) 
:: State s b
\x -> (runState (fst $ fn x)) x 
          where fn = runState (g <$> (State f))

Correct implementation.
We apply the first element of the tuple returned from the first function application to the second element of the tuple returned from the first function application.

\x -> (runState (fst ((runState (g <$> (State f))) x))) (snd ((runState (g <$> (State f))) x)) 

\x -> (runState (fst $ fn x)) (snd $ fn x) 
  where fn = runState (g <$> (State f)) 

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
runState (modify (+1)) 0
We seem to need a Monad instance for this one to work:
runState (modify (+1) >> modify (+1)) 0 

Breakdown of the above function
State $ \x -> ((), f x) >> State $ \x -> ((), f x)
\x -> ((), f x)
  \y -> ((), f y)
    where y = f x

Understanding >>= when typed to (modify (+1))

\x -> (runState (fst ((runState (g <$> (State f))) x))) (snd ((runState (g <$> (State f))) x)) 

(>>=) :: State s () -> (() -> State s ()) -> State s ()
State f >>= g 

g <$> (State f) = 
  (() -> State s ()) <$> State $ \x -> ((), x + 1)
  State $ \x -> (State s (), x + 1)

runState $ g <$> (State f) = 
  \x -> (State s (), x + 1)

runState $ (g <$> (State f)) x = 
  (State s (), x + 1)

fst (State s (), x + 1) = 
  State s ()

snd (State s (), x + 1) = 
  x + 1

runState $ State s () = 
  \x -> ((), x + 1)

runState $ State s () $ x + 1 =
  \x -> ((), x + 1) 

((), (x + 1) + 1)


-}