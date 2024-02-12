import Data.Char (toLower)

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = small ++ [x] ++ big
  where
    small = qsort [a | a <- xs, a <= x]
    big = qsort [a | a <- xs, a > x]

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter isAnagram xss
  where
    isAnagram ys = qsort (map toLower xs) == qsort (map toLower ys) && xs `notElem` xss