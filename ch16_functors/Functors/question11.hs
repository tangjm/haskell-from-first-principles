module Question11 where 

data TalkToMe a =
    Halt 
  | Print String a
  | Read (String -> a)

-- instance Functor TalkToMe where
--   fmap f Halt = Halt
--   fmap f (Print xs a) = Print xs (f a)
--   fmap f (Read g) = Read (f . g)

instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print xs a) = Print xs (f a)
  fmap f (Read g) = Read (fmap f g)

-- In some sense, if a Functor is a function g, namely, something of type (->), then fmap and function composition amount to the same thing.

-- f = Read read

-- main :: IO ()
-- main = do
--   print $ fmap (+1) (f "1")
