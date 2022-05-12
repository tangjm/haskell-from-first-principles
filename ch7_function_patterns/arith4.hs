module Arith4 where 

-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read a) => a -> a  
-- roundTrip :: (Show a, Read b) => a -> b
-- Why doesn't this type signature work?
-- Because 'read' cannot infer the datatype of the parsed String argument
roundTrip a = read (show a)

roundTripPF :: (Show a, Read a) => a -> a 
roundTripPF = read . show

roundTripTwoArgs :: (Show a, Read b) => a -> b  
roundTripTwoArgs = read . (show :: (Show a, Read b) => a -> [b])

main :: IO ()
main = do 
  print (roundTrip 4)
  print (id 4)