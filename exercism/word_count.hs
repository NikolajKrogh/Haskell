import Data.Char (toLower)

groupEqualElements  :: Eq a => [a] -> [[a]]
groupEqualElements  [] = []
groupEqualElements  (x : xs) = (x : takeWhile (== x) xs) : groupEqualElements  (dropWhile (== x) xs)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = small ++ [x] ++ big
  where
    small = qsort [a | a <- xs, a <= x]
    big = qsort [a | a <- xs, a > x]

replaceCommasWithSpaces  :: String -> String
replaceCommasWithSpaces  xs = [if x == ',' then ' ' else x | x <- xs]

isAlphaNumOrQuote  :: Char -> Bool
isAlphaNumOrQuote  char
  | (char >= 'a' && char <= 'z') = True
  | (char >= 'A' && char <= 'Z') = True
  | (char >= '0' && char <= '9') = True
  | (char == ' ') = True
  | (char == '\'') = True
  | otherwise = False

-- removes first the leading quote and reverse to remove the trailing quote and reverse again to get the correct order
removeQuotes :: String -> String
removeQuotes = dropWhile (== '\'') . reverse . dropWhile (== '\'') . reverse

-- Convert the string to lowercase
toLowercase :: String -> String
toLowercase = map toLower

-- Filter out non-alphanumeric characters (except single quotes)
filterChars :: String -> String
filterChars = filter isAlphaNumOrQuote

-- Remove leading and trailing single quotes from each word
removeQuotesFromWords :: String -> String
removeQuotesFromWords = unwords . map removeQuotes . words

-- Combine all the steps to normalize the text
normalizeText :: String -> String
normalizeText = removeQuotesFromWords . filterChars . replaceCommasWithSpaces . toLowercase

-- Normalize the text and split it into words
normalizeAndSplit :: String -> [String]
normalizeAndSplit = words . normalizeText

-- Sort and group equal elements
sortAndGroup :: [String] -> [[String]]
sortAndGroup = groupEqualElements . qsort

-- Count the occurrences of each word
countOccurrences :: [[String]] -> [(String, Int)]
countOccurrences = map (\words -> (head words, length words))

-- Combine all the steps to count the words
wordCount :: String -> [(String, Int)]
wordCount = countOccurrences . sortAndGroup . normalizeAndSplit