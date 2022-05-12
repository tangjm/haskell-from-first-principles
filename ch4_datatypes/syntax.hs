module Syntax where

f xs = w + 1
  where w = length xs

identity = \x -> x

zeroth = \xs -> head xs

project (a, b) = a