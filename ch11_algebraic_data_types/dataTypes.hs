module DataTypes where

data Weekday = 
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

f Friday = "Miller Time"

g xs = xs !! (length xs - 1)