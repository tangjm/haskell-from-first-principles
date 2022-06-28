module ZipListApplicative where

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a 
take' = undefined

instance Functor List where 
  fmap _ Nil = Nil
  fmap f (Cons x tail) = Cons (f x) (fmap f tail)

instance Applicative List where
  pure = flip Cons Nil
  (<*>) = undefined




append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys 

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a 
concat' = fold append Nil 

flatMap :: (a -> List b) -> List a -> List b 
flatMap f as = concat' $ f <$> as

{- flatMap example 

flatMap pure (Cons 2 Nil) = 
  concat' $ fmap pure (Cons 2 Nil)
  concat' $ Cons (pure 2) (fmap pure Nil)
  concat' $ Cons (Cons 2 Nil) Nil
  fold append Nil $ Cons (Cons 2 Nil) Nil
  append (Cons 2 Nil) (fold append Nil Nil)
  append (Cons 2 Nil) Nil
  Cons 2 $ Nil `append` Nil
  Cons 2 Nil 
-}





{- Examples.

fold append Nil (Cons (Cons 2 Nil) Nil) = 
  append (Cons 2 Nil) (fold append Nil Nil)
  append (Cons 2 Nil) Nil 
  Cons 2 $ Nil `append` Nil
  Cons 2 Nil

fold append Nil (Cons 2 Nil) = append 2 (fold append Nil Nil)
                             = append 2 Nil

fold append Nil (Cons 2 (Cons 3 Nil)) = 
  append 2 (fold append Nil (Cons 3 Nil))
  append 2 (append 3 (fold append Nil Nil))
  append 2 (append 3 Nil)
-}









newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

-- instance Eq a => EqProp (ZipList' a) where
--   xs =-= ys = xs' `eq` ys'
--     where xs' = let (ZipList' l) = xs
--                 in take' 3000 l
--           ys' = let (ZipList' l) = ys
--                 in take' 3000 l

instance Functor ZipList' where 
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = undefined
  (<*>) = undefined



