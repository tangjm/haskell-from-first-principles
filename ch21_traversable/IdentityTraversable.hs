module IdentityTraversable where

import Data.Functor.Compose
import qualified Data.Functor.Identity as Id
import Data.Functor.Classes (Eq1)

newtype Identity a = Identity a  
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where 
  traverse f (Identity x) = Identity <$> f x

-- We also need to define instances of the Functor and Foldable typeclasses for Identity because Traversables are also Functors and Foldables.

-- Laws for the 'traverse' function
naturality :: (Applicative f) => (f b -> c) -> (a -> f b) -> a -> Bool
naturality t f x = (t . traverse f) (Identity x) == traverse (t . f) (Identity x)

{- 
Proof of naturality
LHS

(t . traverse f) (Identity x)
t $ traverse f (Identity x)
t $ Identity <$> f x
let (f x) = f' x'
t $ f' (Identity x')
t (f' x')

RHS

traverse (t . f) (Identity x)
Identity <$> (t . f) x
Identity <$> t $ f x
let (f x) = f' x'
Identity <$> t $ f' x'
t $ Identity (f' x'))
forall a . Identity a == a
t (f' x')
-}

identity :: Eq a => a -> Bool
identity x = traverse Id.Identity (Identity x) == Id.Identity (Identity x)

-- n.b. Id.Identity and Identity denote the same type constructor. The different names are there to make the distinction between the default implementation provided in Data.Functor.Identity and our own implementation.
{-
Proof of identity
LHS

Identity <$> Identity x 
Identity $ Identity x
Identity x
x

RHS
Identity $ Identity x
Identity x
x
-}

composition :: (Eq1 f, Eq1 g, Eq c, Applicative f, Applicative g) => (a -> f b) -> (b -> g c) -> a -> Bool 
composition f g x = traverse (Compose . fmap g . f) (Identity x)
              == (Compose . fmap (traverse g) . traverse f) (Identity x)

{-
Proof of composition
RHS

Compose . fmap (traverse g) . traverse f $ (Identity x)

Compose . fmap (traverse g) $ traverse f (Identity x)
Compose . fmap (traverse g) $ Identity <$> f x
let (f x) = f' x'
Compose . fmap (traverse g) $ Identity <$> f' x' 
Compose . fmap (traverse g) $ f' (Identity x')
Compose $ fmap (traverse g) (f' (Identity x'))
Compose $ f' (traverse g (Identity x'))
Compose $ f' (Identity <$> g x')
let (g x') = g' x''
Compose $ f' (g' (Identity x''))
Compose f' (g' (Identity x''))
-}


{- 
LHS 

traverse (Compose . fmap g . f) (Identity x)

Identity <$> (Compose . fmap g . f) x
Identity <$> (Compose . fmap g) $ f x
let (f x) = f' x'
Identity <$> (Compose . fmap g) $ f' x'
Identity <$> Compose $ fmap g (f' x')
Identity <$> Compose $ f' (g x')
let (g x') = g' x''
Identity <$> Compose $ f' (g' x'')
Identity <$> Compose f' (g' x'')
Now because Compose has arity-3, Compose isn't a functor, but the partially applied (Compose f' g') is indeed a functor. Hence, we get:
Compose f' g' (Identity x'')

-}



main :: IO ()
main = undefined