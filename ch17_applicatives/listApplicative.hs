module ListApplicative where

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x tail) = Cons (f x) (fmap f tail)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys  

instance Applicative List where
  pure = flip Cons Nil
  Nil <*> Nil = Nil
  Nil <*> _ = Nil 
  _ <*> Nil = Nil
  Cons f tail <*> Cons x tail' = Cons (f x) (fmap f tail' `append` (tail <*> Cons x tail'))

functions = Cons (+1) (Cons (*2) Nil)
values = Cons 1 (Cons 2 Nil)
y = functions <*> values
-- expect y to be: Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))


-- Cons (f x) (pure f <*> tail) = Cons 2 (Cons 3 Nil)
{- Plan
Cons (+1) (Cons (*2) Nil) <*> Cons 1 (Cons 2 Nil) =
  Cons ((+1) 1) (Cons ((+1) 2) (Cons ((*2) 1) (Cons ((*2) 2) Nil)))

Cons (f x) (fmap f tail' <*> (tail <*> Cons x tail'))
But this doesn't work because (fmap f tail') doesn't have type List (a -> a).
We can try using (<>) instead of (<*>), but that would require an instance of Semigroup.
We need a way to turn (Cons 3 Nil `f` Cons 2 (Cons 4 Nil)) into (Cons 3 (Cons 2 (Cons 4 Nil))).
This can be done by taking the function `f` to be `append`.
-}

x = Cons 2 Nil



getTestResult :: Bool -> String
getTestResult True = "Ok"
getTestResult False = "Fail"

main :: IO ()
main = do
  print $ "Identity: "++ if (pure id <*> x) == (id x) then "Ok" else "Fail"
  print $ ("Composition: "++) . getTestResult $ (pure (.) <*> pure (+1) <*> pure (*2) <*> x) == (pure (+1) <*> (pure (*2) <*> x))
  -- print $ ("Homomorphism "++) . getTestResult $ (pure id <*> pure 2) == (pure (id 2))
  -- print $ (pure 2)

-- pure f <*> pure x = pure (f x)