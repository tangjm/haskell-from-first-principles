module Playground where


f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (x, _, z) (l, _, m) = ((x, l), (z, m))


g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)

