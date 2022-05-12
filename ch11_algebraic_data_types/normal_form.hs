module NormalForm where

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac 
                deriving Show

type Gardener = String

data Garden = 
  Garden Gardener FlowerType
  deriving Show

-- |Gardener| * (|Gardenia| + |Daisy| + |Rose| + |Lilac|)

-- this is in normal form or 'sum of products'
-- data Garden =
--   FlowerType Gardener
--   deriving Show