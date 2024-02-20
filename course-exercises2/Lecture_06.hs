-- Exercise 1
within :: (Ord a) => [a] -> (a, a) -> [a]
within xs (a, b) = filter (<= b) $ filter (>= a) xs

-- Exercise 2
sumrows :: (Num a) => [[a]] -> [a]
sumrows [] = []
sumrows xs = map sum xs

-- Exercise 3
fact k = product [1 .. k]

approx n = sum (map (\factorialArg -> 1 / fact factorialArg) [0 .. n])

-- Exercise 4
fingo :: [a] -> [a] -> [a]
fingo xs ys = foldr (:) xs ys

-- a)
partitionFilter :: (a -> Bool) -> [a] -> ([a], [a])
partitionFilter predicate xs = (filter predicate xs, filter (not . predicate) xs)

partionFoldr :: (a -> Bool) -> [a] -> ([a], [a])
-- So, ([], []) is needed as the starting point for this process. Without it, foldr wouldn't know what value to start with.
partionFoldr predicate xs = foldr (\x (yes, no) -> if predicate x then (x : yes, no) else (yes, x : no)) ([], []) xs

-- b)
filterUsingFoldr :: (Eq a) => (a -> Bool) -> [a] -> [a]
filterUsingFoldr predicate xs = foldr (\x filteredSoFar -> if predicate x then (x : filteredSoFar) else filteredSoFar) [] xs

-- c)
remove :: String -> String -> String
remove xs (y : ys) = filter (`notElem` xs) ys

removefoldr :: String -> String -> String
removefoldr xs ys = foldr(\x filteredSoFar -> if x `elem` xs then filteredSoFar else x : filteredSoFar) [] ys

-- d)

count :: (Eq a) => a -> [a] -> Int
count element xs = length [x | x <- xs, x == element]

min2 :: (Ord a) => [a] -> a
min2 xs = if count minValue xs > 1 then minValue else minValue2 
  where
    minValue = foldr1 (min) xs
    minValue2 = foldr1 min ( filter (/=minValue) xs )
