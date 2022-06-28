module LiftingExercises where

a = (+1) <$> read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- We map (*2) over a function here as functions are also Functors. Note that we only map over the return argument of (->) even though it is a binary data constructor.
c = fmap (*2) (\x -> x - 2)

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer  
        changed = read <$> ("123"++) <$> show <$> ioi
        -- changed = read <$> (("123"++) <$> (show <$> ioi))
        -- changed = fmap read (fmap ("123"++) (fmap show ioi))
    in fmap (*3) changed

-- show <$> ioi gives us an IO String, namely, IO "1"
-- fmap ("123"++) (fmap show ioi) prepends "123" to our string, giving us IO "1231"
-- fmapping 'read' over IO "1231" converts "1231" into a readable, so we have, changed :: Read a => IO a.
-- Finally, 'fmap (*3) changed' converts 'a' to an Integer, performs the calculation (1231 * 3) and returns IO 3693, which has type IO Integer.



