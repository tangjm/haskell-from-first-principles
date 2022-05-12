module RemoveArticles where

isArticle :: [Char] -> Bool
isArticle xs
  | xs == "the" = False
  | xs == "a"   = False
  | xs == "an"  = False
  | otherwise   = True

main :: IO ()
main = print $ filter isArticle $ words "the brown dog was a goof"