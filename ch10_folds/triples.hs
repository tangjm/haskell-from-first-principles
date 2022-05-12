module Triples where 

nouns :: [String]
nouns = ["jared", "kant", "hume", "plato", "aristotle", "hilbert", "fichte"]

verbs :: [String]
verbs = ["likes", "dislikes", "admires", "agrees with"]

triples :: [(Char, Char, Char)]
triples = map (\x -> (fst . fst $ x, snd . fst $ x, snd x)) xs 
  where xs = [(x, y) | x <- [(x', x'') | x' <- "pbtdkg", x'' <- "aeiou" ], y <- "pbtdkg"]

triplesOnlyP :: [(Char, Char, Char)]
triplesOnlyP = map (\x -> (fst . fst $ x, snd . fst $ x, snd x)) . filter (\x -> (=='p') . fst . fst $ x) $ xs 
  where xs = [(x, y) | x <- [(x', x'') | x' <- "pbtdkg", x'' <- "aeiou" ], y <- "pbtdkg"]

simpleTriples :: [(Char, Char, Char)]
simpleTriples = [(a, b, c) | a <- "pbtdkg", b <- "aeiou", c <- "pbtdkg"]

sentenceTriples :: [String] -> [String] -> [(String, String, String)]
sentenceTriples nouns verbs = map (\x -> (fst . fst $ x, snd . fst $ x, snd x)) xs 
  where xs = [(x, y) | x <- [(x', x'') | x' <- nouns, x'' <- verbs ], y <- nouns]

main :: IO ()
main = print $ sentenceTriples nouns verbs