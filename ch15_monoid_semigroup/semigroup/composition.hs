module Composition where 

newtype Comp a = 
  Comp { unComp :: (a -> a) } 

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

-- Concrete use case
f = Comp (++"est")
g = Comp (++"cool")

main :: IO ()
main = do 
        putStr "Please enter your name: "
        name <- getLine
        putStrLn . unComp (f <> g) $ name ++ " is the "
