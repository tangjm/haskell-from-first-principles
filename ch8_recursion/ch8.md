# Recursion

The fixed-point of a function is a value mapped to itself by the function.

E.g. f(x) = x

```haskell
brokenFact :: Integer -> Integer
brokenFact n = n * brokenFact1 (n - 1)

4 * brokenFact1(4 - 1)
4 * (4 - 1) * brokenFact((4 - 1) - 1)
4 * (4 - 1) * ((4 - 1) - 1) * brokenFact(((4 - 1) - 1) - 1)
4 * (4 - 1) * ((4 - 1) - 1) * (((4 - 1) - 1) - 1)
```

```haskell
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n - 1) f b)

alternatively,
applytTimes n f b = f. applyTimes (n - 1) f $ b

-- nested function applications
applyTimes 5 (+1) 5 = (+1) (applyTimes 4 (+1) 5)
                    = (+1) ((+1) (applyTimes 3 (+1) 5))
                    = (+1) ((+1) ((+1) (applyTimes 2 (+1) 5)))
                    = (+1) ((+1) ((+1) ((+1) (applyTimes 1 (+1) 5))))
                    = (+1) ((+1) ((+1) ((+1) ((+1) (applyTimes 0 (+1) 5)))))
                    = (+1) ((+1) ((+1) ((+1) ((+1) 5))))

-- with composition (.) syntax
applyTimes 5 (+1) 5 = (+1) . applyTimes 4 (+1) $ 5
                    = (+1) . (+1) . applyTimes 3 (+1) $ 5
                    = (+1) . (+1) . (+1) . applyTimes 2 (+1) $ 5
                    = (+1) . (+1) . (+1) . (+1) . applyTimes 1 (+1) $ 5
                    = (+1) . (+1) . (+1) . (+1) . (+1) . applyTimes 0 (+1) $ 5
                    = (+1) . (+1) . (+1) . (+1) . (+1) $ 5
```

Bottom or $\bot$ refers to computations that do not successfully result in a value. Usually because of an error or an unbounded computation.

Recursive definition of division in terms of subtraction

```haskell
type Dividend = Integer
type Divisor = Integer
type Quotient = Integer
type Remainder = Integer

dividedBy :: Dividend -> Divisor -> Remainder
dividedBy n m = 
  | n < m     = n
  | otherwise = dividedBy (n - m) m

dividedBy 25 5 = 25 - 5, 20
                    - 5, 15
                    - 5, 10
                    - 5, 5
                    - 5, 0

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denum count
  where go n d count
        | n < d = (count, n)
        | otherwise = go (n - d) d (count + 1)

```

8.6 Chapter exercises

1d
2b
3d
4b

Reviewing currying

```haskell
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"
```

1. "woops mrow woohoo!"
2. "1 mrow haha"
3. "woops mrow 2 mrow haha"
4. "woops mrow blue mrow haha"
5. "pink mrow haha mrow green mrow woops mrow blue"
6. "are mrow Pugs mrow awesome"

Recursion

1.
```haskell
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d  = (count, n)
          | otherwise = go (n - d) d (count + 1)

-- Write out steps that reduces the follow function application to its final value.
dividedBy 15 2 = go 15 2 0
               = go 13 2 1
               = go 11 2 2
               = go 9 2 3
               = go 7 2 4
               = go 5 2 5
               = go 3 2 6
               = go 1 2 7
               = | 1 < 2 
               = (7, 1)
```

```haskell
-- sum numbers from 1 to n
sum' :: (Eq a, Num a) => a -> a
sum' 1 = 1
sum' n = n + sum (n - 1)
```

```haskell
-- define multiplication recursively in terms of addition
mult :: Integral a => a -> a -> a
mult 0 m = 0
mult n m = mult (n - 1) m + m
```

Fixing dividedBy

```haskell
data DividedResult = 
    Result Integer
  | DividedByZero

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy _ 0 = DividedByZero
dividedBy num
dividedBy num denom = go num denom 0
  where go n d count
          | n < d  = (count, n)
          | otherwise = go (n - 1) d (count + 1)
```


McCarthy 91 function
$$
MC(n) =
\begin{cases}
  n - 10         & \text{if  } n > 100 \\
  MC(MC(n + 11)) & \text{if  } n <= 100
\end{cases}
$$

```haskell
MC(98) = MC(MC(98 + 11))
       = MC(MC(109))
       = MC(109 - 10)
       = MC(99)
       = MC(MC(99 + 11))
       = MC(MC(110))
       = MC(110 - 10)
       = MC(100)
       = MC(MC(100 + 11))
       = MC(MC(111))
       = MC(111 - 10)
       = MC(101)
       = 91

MC(99) = MC(MC(99 + 11))
       = MC(MC(110))
       = MC(100)
       = MC(MC(111))
       = MC(101)
       = 101 - 10
       = 91

MC(100) = MC(MC(111))
        = MC(101)
        = 101 - 10
        = 91

MC(105) = 95
MC(110) = 100

mc91 :: Integer -> Integer
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 . mc91 $ n + 11
```

Numbers into words

```haskell
-- getting the digits from an integer
units = mod n 10
tens = mod (div n 10) 10
hundreds = mod (div (div n 10) 10) 10
...
```

We can recusively define this in terms of the number of `div` function applications.

Examples
```haskell

n = 1001
mod 1001 10 = 1 --units
div 1001 10 = 100 -- mod 100 10 = 0 (tens)
div 100 10 = 10 -- mod 10 10 = 0 (hundreds)
div 10 10 = 1 -- thousands
div 1 10 = 0

-- Because there might be stages of the recursive call where (div n 10) produces a result that is greater than 9, we can resort to `mod (div n 10) 10`

mod 1001 10 = 1 --units
mod (div 1001 10) 10 = 0 -- mod 100 10 = 0 (tens)
mod (div 100 10) 10 = 0 -- mod 10 10 = 0 (hundreds)
mod (div 10 10) 10 = 1 -- thousands
div 1 10 = 0

-- For our purposes, we create new lists on each recursive call.
-- Then once we reach the base case, namely, when `div n 10 == 0`, we simply return the current list at that point.


```haskell
digits :: Int -> [Int]
digits n = go n [mod n 10]
  where go n digitList
          | div n 10 == 0  = digitList
          | otherwise      = go (div n 10) ((:digitList) $ flip mod 10 $ div n 10)

```

See wordNumber.hs for the rest of the implementation.