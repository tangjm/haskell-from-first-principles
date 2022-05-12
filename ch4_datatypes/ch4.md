1. length :: [a] -> Int
2. 
a) 5
b) 3
c) 2
d) 1


```haskell
Q7.
-- True
length allAwesome == 2 

length [1, 'a', 3, 'b']
-- error because lists cannot have different data types

length allAwesome + length awesome
-- works fine

(8 == 8) && ('b' < 'a')
-- False

(8 == 8) && 9
-- error, 9 isn't of typeclass Bool

```


```haskell
Q10.
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

```


```haskell
x = (+)
f xs = w x 1
  where w = length xs
```

pp.128-129
1. c
2. b 
3. a
4. d
