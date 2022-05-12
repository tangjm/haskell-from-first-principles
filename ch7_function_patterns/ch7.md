# ch7

p.228

```haskell
1.
all equivalent

2d.

3a 
f = \n -> n + 1

3b
addFive = \x -> \y -> (if x > y then y else x) + 5

3c
mflip f x y = f y x

```

Exercises on pattern matching

```haskell
k :: (a, b) -> a
k1 :: Num a => a
k2 :: [Char]
k3 :: Num a => a
```
1a. k :: (a, b) -> a
1b. Different to both k1 and k3
1c. k3

2.
```haskell
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (x, _, z) (l, _, m) = ((x, l), (z, m))

```

Guards provide a compact way to define functions in a way similar to how we would define a function through a definition by cases.

Guards are evaluated sequentially in lexical order.


Exercises on Guards

```haskell

-- pal :: Eq a => [a] -> Bool 
pal xs  
    | xs == reverse xs = True 
    | otherwise        = False

numbers :: (Ord a, Num a, Num p) => a -> p
numbers x
    | x < 0  = -1
    | x == 0 = 0
    | x > 0  = 1

```

see how more readable composition (.) syntax is
1. i (h (f (g x)))
2. i . h . f . g $ x


Pointfree style 
A style of composing functions without specifying their arguments. This styles draws your attention to the composed functions rather than the data passed around.

```haskell
f x = take 5 . filter odd . enumFrom $ x

-- Pointfree style
f = take 5 . filter odd . enumFrom
f 3
numOfAs = length . filter (== 'a')

```

print is really a composition of two functions:
```haskell
show :: Show a => a -> String
putStrLn :: String -> IO ()
print :: Show a => a -> IO ()

print = putStrLn . show
```

7.11 Chapter exercises

1-5 are trivial

Let's write code
2. 
```haskell
foldBool :: a -> a -> Bool -> a
foldBool x y True = x
foldBool x y False = y

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y b = 
    case b of
        True -> x
        False -> y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y b 
    | b         = x
    | otherwise = y
```

```haskell
3.
g :: (a -> b) -> (a, c) -> (b, c)
-- composition
g f x = (f . fst $ x, snd x)

-- with pattern matching
g f (x, y) = (f x, y)
```

4. See Arith4.hs

Think of `show` and `read` functions as JSON.stringify and JSON.parse respectively.


