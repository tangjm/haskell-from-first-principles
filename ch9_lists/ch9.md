# Lists (singly-linked lists)

Type constructor
```haskell
data [] a = [] | a : [a]
```

Can think of these as linked-lists

```haskell
[1..3] is constructed in this way:
1 : (2 : (3 : []))
```

```haskell
myWords :: [Char] -> [[Char]]
myWords s = go s []
  where go s newList
          | takeWhile f s == [] = newList
          | takeWhile f s == s  = newList ++ [s]
          | otherwise           = go (dropWhile f' . dropWhile f $ s) (newList ++ [takeWhile f s])
          where f  = (/= ' ')
                f' = (== ' ')

myLines :: String -> [String]
myLines poem = go poem []
  where go poem lines 
          | takeWhile f poem == []   = lines
          | takeWhile f poem == poem = lines ++ [poem]
          | otherwise                = go (dropWhile f' . dropWhile f $ poem) (lines ++ [takeWhile f poem])
          where f = (/= '\n')
                f' = (== '\n')

splitSentenceBy :: [Char] -> Char -> [[Char]]
splitSentenceBy s c = go s []
  where go s charList
          | takeWhile f s == s = charList ++ [s]
          | otherwise          = go (dropWhile f' . dropWhile f $ s) (charList ++ [takeWhile f s])
          where f = (/= c)
                f' = (== c)
```


### Spines and non-strict evaluation

Normal Form (NF) refers to fully evaluated values
Weak Head Normal Form (WHNF) refers to values evaluated to the point of reaching a data constructor or a lambda awaiting an argument.

Evaluation of a list goes down the spine or the `(:)` operators. 

Construction of a list goes up the spine starting from the empty list `[]`.

Spine-strict functions are functions that force complete evaluation of the spine of a data structure.

Pattern matching is by default spine-strict and can also force complete evaluation of the evaluation in certain contexts.

The `length` function is another example of a spine-strict function that is not value-strict. To see this, the length function will still return the length of a list with bottom/undefined elements. It doesn't force the evaluation of the values that inhabit the `(:)`.

E.g. 
```haskell
x = [1, 2, undefined, 3]
length x --would still return 4
:sprint x --would return an unevaluated x, namely, x = _
```

mySum is a function that forces both the evaluation of the spine and values residing/inhabiting each 'cons' cell or `(:)`.

This is because the evalution of the (+) operator depends on the evaluation of its operands. And since mySum involves recursive applications of (+) to all elements of its list argument, for mySum to return a value, all values in the `cons` of its list argument must be evaluated.

```haskell
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs
```
How evaluation works for an example case: mySum [1..5]

```haskell
mySum [1..5] = 1 + mySum [2..5]
             = 1 + (2 + mySum [3..5])
             = 1 + (2 + (3 + mySum [4..5]))
             = 1 + (2 + (3 + (4 + mySum [5])))
             = 1 + (2 + (3 + (4 + (5 + mySum [])))) 
             = 1 + (2 + (3 + (4 + (5 + 0))))
             = 1 + (2 + (3 + (4 + 5)))
             = 1 + (2 + (3 + 9))
             = 1 + (2 + 12)
             = 1 + 14
             = 15
```

p321 - question 6

```haskell
-- equivalent anonymous lambdas 
(\x -> if x == 3 then (-x) else x)
(\x -> bool (x) (-x) (x == 3))
```

Multiples of three from a list 1-30

```haskell
-- higher-order filter function
filter (\x -> rem x 3 == 0) [1..30]
-- list comprehension
[ x | x <- [1..30], rem x 3 == 0]]

-- Number of elements that are multiples of 3 from 1 to 30
length . filter (\x -> rem x 3 == 0) $ [1..30]

```


```haskell
-- A function that takes a sentence and returns a list of words in the sentence after removing definite and indefinite articles ('the', 'a', 'an').

filter :: (a -> Bool) -> [a] -> [a]

filter isArticle [] = []
filter isArticle (x:xs)
  | isArticle(x) = filter isArticle xs
  | otherwise    = x : filter isArticle xs

isArticle :: [Char] -> Bool
isArticle xs
  | xs == "the" = True
  | xs == "a"   = True
  | xs == "an"  = True
  | otherwise 	= False

split :: [Char] -> [[Char]]
split [] = []
split xs = go xs []
  where go xs newList
	  | takeWhile (/= ' ') xs == xs = newList ++ xs
	  | otherwise 			            = go (dropWhile (== ' ') (dropWhile (/= ' ') xs)) (newList ++ takeWhile (/= ' ') xs)
```


My version of zip

```haskell
myZip :: [a] -> [b] -> [(a, b)]

-- formal description of what the 'zip' function does
-- a list of ordered pairs <n, m> where n is the ith element of [a] and m is the ith element of [b]
-- where i is at most min (length n) (length m)

-- We can let i be the max number of recursive calls
-- For our base case, if either [a] or [b] is [], then we return []

myZip _ [] = []
myZip [] _ = []
myZip xs ys = go xs ys [] i
  where go xs ys tupleList callCount
          | callCount == 0 = tupleList
          | otherwise      = go (tail xs) (tail ys) (tupleList ++ [(head xs, head ys)]) callCount - 1
  where i
    | length xs < length ys = length xs
    | otherwise             = length ys

-- the use of 'i' or 'callCount' would be an imperative way of writing this
-- Instead, let's stop the recursive calls when either xs or ys is empty

myZip xs ys = go xs ys []
  where go xs ys tupleList
          | xs == [] || ys == [] = tupleList
          | otherwise            = go (tail xs) (tail ys) (tupleList ++ [(head xs, head ys)])

-- We can't do this as this assumes that xs and ys have elements that are data types constrained by the Eq typeclass, but our type signature has no typeclass constraints.

myZip xs ys = go xs ys []
  where go xs ys tupleList
          | length xs == 0 || length yx == 0 = tupleList
          | otherwise                        = go (tail xs) (tail ys) (tupleList ++ [(head xs, head ys)])

-- alternatively,
myZip [] _ = []
myZip _ [] = []
myZip xs ys = [(head xs, head ys)] ++ myZip (tail xs) (tail ys)

-- alternatively,
myZip [] _ = []
myZip _ [] = []
myZip xs ys = (head xs, head ys) : myZip (tail xs) (tail ys)

-- Example:
myZip [1] [2] = go [1] [2] [] 1 
            = go [] [] ([] ++ [(1, 2)]) 0
            = go [] [] [(1, 2)] 0
            = [(1, 2)]
```

My version of zipWith
```haskell
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f xs ys = f (head xs) (head ys) : myZipWith f (tail xs) (tail ys)


myZipFromZipWith :: [a] -> [b] -> [c]
myZipFromZipWith = myZipWith (,)
```

### Chapter exercises p.325

Capitalise the first letter of a string
```haskell
capitaliseFirst :: String -> String
capitaliseFirst xs = toUpper (head xs) : tail xs

-- write this function using map
```

Make capitaliseFirst recursive so it capitalises each letter of a string

```haskell
capitaliseFirstRecursive :: String -> String
capitaliseFirstRecursive [] = []
capitaliseFirstRecursive xs = toUpper (head xs) : capitaliseFirstRecursive (tail xs)

-- alternatively,
capitaliseFirstRecursive (x : xs) = toUpper x : capitaliseFirstRecursive xs

-- example: capitaliseFirstRecursive "ab"
(toUpper (head "ab") : (toUpper (head "b") : []))

-- We can also use the 'map' function
capitaliseFirstRecursive xs = map toUpper xs
```

Return the capitalised first letter of a string

```haskell
getFirstCapitalised :: String -> Char
getFirstCapitalised xs = toUpper $ head xs

-- rewrite as composed function
head :: [a] -> a
toUpper :: Char -> Char

getFirstCapComposed :: String -> Char
getFirstCapComposed xs = toUpper . head $ xs

getFirstCapPointFree :: String -> Char
getFirstCapPointFree = toUpper . head
```

Writing a caesar cipher - see [caesar cipher](./cipher.hs)

Implementations of Standard Library functions - [std functions](./myStdFuncs.hs)

```haskell
maximumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy :: (a -> a -> Ordering) -> [a] -> a

-- How these functions work
-- the first argument, (a -> a -> Ordering), is applied to the first two elements of [a] and then depending on the ordering returned, one of the two arguments to (a -> a -> Ordering) will be passed to the next application of the (a -> a -> Ordering) function as the first argument; the third element of [a] will serve as the second argument. This then continues until we reach the end of the list [a].

maximumBy (\x y -> GT) [1..10]
-- the value of x in the final application of (\x y -> GT) will be returned.
-- returns 1

-- because (\x y -> GT) always returns GT, the first argument to the first application of this function, namely, x, will be retained after the first function application for all subsequent applications of (\x y -> GT). Finally, the value of x in the final application of (\x y -> GT) will be returned. Now since x remains the same for all subsequent applications of (\x y -> GT) and the final application of (\x y -> GT) is one such subsequent application of (\x y -> GT), x or head [1..10] will be what's returned.

maximumBy (\x y -> EQ) [1..10]
-- returns 10
maximumBy (\x y -> LT) [1..10]
-- returns 10

-- More generally, when evaluating maximumBy, 
(\x y -> GT) -- x is carried over to the next function application
(\x y -> EQ) -- y is carried over to the next function application
(\x y -> LT) -- y is carried over to the next function application

-- In sum, for our maximumBy function,
-- What's returned is x in the final application of (\x y -> Ordering) if Ordering is GT, otherwise y is returned

minimumBy (\x y -> GT) [1..10]
-- With this function, we return the value of y in the final application of (\x y -> GT).
-- returns 10
minimumBy (\x y -> EQ) [1..10]
-- returns 1
minimumBy (\x y -> LT) [1..10]
-- returns 1

-- When evaluating minimumBy,
(\x y -> GT) -- y is carried over to the next function application
(\x y -> EQ) -- x is carried over to the next function application
(\x y -> LT) -- x is carried over to the next function application

-- In sum, for minimumBy,
-- What's returned is x in the final application of (\x y -> Ordering) if Ordering is LT or EQ, otherwise y is returned.
```



