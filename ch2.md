The first declaration in a module determines at which column each subsequent declaration in the module must start at.



`-` is overloaded. It could denote `negate` or subtraction.

Wrap infix operators inside parentheses to use them as prefix operators.
E.g. 
```haskell
(+) 2 3 gives 5
```

Sectioning allows us to pass around partially applied functions.
```haskell
(/2) 1 equates to 1 / 2 == 0.5
(2/) 1 equates to 2 / 1 == 2.0
```

When using sectioning with subtraction, you can only partially apply the first argument.
The '-' in `(-2)` would be interpreted as negation applied to a single argument, namely, 2, rather than subtraction.

(2-) 3 would give -1


Adding parentheses

```haskell
-- 2 + 2 * 3 - 1
2 + (2 * 3) - 1

-- (^) 10 $ 1 + 1
(^) 10 $ (1 + 1)

-- 2 ^ 2 * 4 ^ 5 + 1
((2 ^ 2) * (4 ^ 5)) + 1
```

Equivalent expressions (pp.85 - 86)

1. yes
2. yes 
3. no
4. no 
5. no 

Chapter Exercises

Reading syntax

```haskell
a. yes
b. no
c. yes
d. no
["hello" ++ " world"] == ["hello world"]
e. no
f. yes - outputs 'o'
g. no - 
h. yes - outputs "awe"
```

```haskell
concat [[1 * 6], [2 * 6], [3 * 6]]
[6, 12, 18]

"rain" ++ drop 2 "elbow"
"rainbow"

10 * head [1, 2, 3]
10

(take 3 "Julie") ++ (tail "yes")
"Jules"

concat [tail [1, 2, 3],
        tail [4, 5, 6],
        tail [7, 8, 9]]
[2, 3, 5, 6, 8, 9]
```

```haskell
1a
exclaim s = s ++ "!"

1b
returnY s = "y"

1c
