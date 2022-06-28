module P678Exercise where

-- 1
x = const <$> Just "Hello" <*> pure "World"

-- 2
y = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- y :: (Num a, Num b, Num c) => Maybe (a, b, [Char], [c])
-- y = Just (90, 10, "Tierness", [1, 2, 3])


