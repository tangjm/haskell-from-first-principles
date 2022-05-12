All you can really do with a parametrically polymorphic value is pass
or not pass it to some other expression. Prove that to yourself with
these small demonstrations.

1. Given the type a -> a, which is the type for id, attempt to make
a function that is not bottom and terminates successfully that
does something other than returning the same value. This is
impossible, but you should try it anyway.

nb. bottom denotes 'undefined'

1. We can get a more comfortable appreciation of parametricity
by looking at a -> a -> a. This hypothetical function a -> a
-> a has two–and only two–implementations. Write both possi-
ble versions of a -> a -> a. After doing so, try to violate the
constraints of parametrically polymorphic values we outlined
above.

```haskell
f :: a -> a -> a
f x y = x
f x y = y
```

3. Implement a -> b -> b. How many implementations can it
have?
Does the behavior change when the types of 𝑎 and 𝑏
change?

```haskell
f :: a -> b -> b
f x y = y
```

### 5.9 Chapter Exercises

Multiple choice

1. c
2. a
3. a, b
4. c

Detemine the type

1a. Num a => a
1b. Num a => (a, [Char])
1c. (Integer, [Char])
1d. Bool
1e. Int
1f. Bool

2. Num a => a
3. Num a => a -> a
4. Fractional a => a
5. [Char]

### -- Does it compile?

```haskell
-- 1.
bigNum = (^) 5 $ 10
wahoo = bigNum $ 10 
-- bigNum isn't a function

-- 2.
x = print 
y = print "woohoo!"
z = x "hello world"
-- compiles fine

-- 3.
a = (+)
b = 5
c = b 10
d = c 200
-- change 'c = b 10' to 'c = a 10'

-- 4.
a = 12 + b
b = 10000 * c
```

### Type variable or specific type constructor?

- fully/paremetrically polymorphic
- constrained/ad-hoc polymorphic
- concrete

2.
'zed' is fully polymorphic
'Zed' is concrete
'Blah' is concrete

3. 'a' is fully polymorphic
'b' is constrained polymorphic
'C' is concrete

4. 'f' and 'g' are fully polymorphic
'C' is concrete

### Write a type signature

```haskell
1.
functionH :: [a] -> a

2.
functionC :: (Ord a, Ord b) => a -> b -> Bool

3.
functionS :: (a, b) -> b
```


### Given a type, write the function

```haskell
1. 
i :: a -> a
i x = x

2.
c :: a -> b -> a
c x y = x

3. 
c'' :: b -> a -> b
c x y = x

4.
c' :: a -> b -> b
c' x y = y

5.
r :: [a] -> [a]
r x = x ++ x
r x = x ++ x ++ x
-- there's infinitely many possibilities

6.
co :: (b -> c) -> (a -> b) -> (a -> c)
co f g x = f (g x)
LET'S RETURN TO THIS LATER

7.
a :: (a -> c) -> a -> a
a x y = y

8.
a' :: (a -> b) -> a -> b
a' x y = x y
a' = ($)
```

### Fix it

```haskell 
module sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"
```


```haskell
-- arithBroken.hs
module ArithBroken where

main :: IO ()
main = do 
  print 1 + 2
  putStrLn 10
  print (negate (-1))
  print ((+) 0 blah)
    where blah = negate 1
```

Type-Kwon-Do

```haskell
1. 
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g f x
```

```haskell
2. 
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w q x
```

```haskell
3. 
data X 
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)
```

```haskell
4.
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g h = fst (g (f h))

munge f :: (y -> (w, z)) -> x -> w
munge f g :: x -> w
munge f g h :: w
```
