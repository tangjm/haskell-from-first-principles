module Question9 where 

data List a =
    Nil
  | Cons a (List a) 
  deriving (Eq, Show)

instance Functor List where 
  fmap _ Nil = Nil
  fmap f (Cons a tail) = Cons (f a) (fmap f tail) 
  

x = Cons 2 (Cons 3 (Cons 4 Nil))

main :: IO ()
main = do
  print $ x
  print $ fmap (+1) x