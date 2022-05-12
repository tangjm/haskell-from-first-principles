module TensDigit where
import GHC.IO.Device (IODevice(dup2))

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

-- tensDigit written using divMod and pointfree style
f :: Integral a => a -> a
f = fst . flip divMod 10


hunsD x = d2
  where d = x `div` 100
        d2 = d `mod` 10

noTensUnits :: Integral a => a -> a
noTensUnits = fst . flip divMod 100

f1 :: Integral a => a -> a
f1 y = (snd . divMod x) 10
  where x = noTensUnits y
                

