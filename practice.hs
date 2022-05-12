module Practice where

-- rewriting 'let' expressions in terms of 'where'

-- let x = 5 in x
identity = x
  where x = 5

-- let x = 5 in x * x
square5 = x * x
    where x = 5

-- let x = 5; y = 6 in x * y
mult1 = x * y
  where x = 5 
        y = 6

-- let x = 3; y = 1000 in x + 3
add3 = x + 3
  where x = 3
        y = 1000

-- 1. let x = 3; y = 1000 in x * 3 + y
mult2 = x * 3 + y
  where x = 3
        y = 1000

-- 2. let y = 10; x = 10 * 5 + y in x * 5
arith = x * 5
  where y = 10
        x = 10 * 5 + y

-- 3. let x = 7; y = negate x; z = y * 10 in z / x + y
arith2 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

waxOn = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7

triple x = x * 3

waxOff x = triple x