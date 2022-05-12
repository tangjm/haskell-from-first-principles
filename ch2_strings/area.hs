module Area where

area d = pi * (r * r)
  where r = d / 2

area' d = let r = d / 2 in pi * (r * r) 