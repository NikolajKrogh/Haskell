{-Prep 1-}
positions :: String -> [Int]
positions [] = []
positions (x : xs) = fromEnum x - 96 : positions xs

{-Prep 2-}
sumsq :: (Integral a) => a -> a
sumsq a = sum [x ^ 2 | x <- [0 .. a]]

sumsq' :: (Integral a) => a -> a
sumsq' n = foldr (\x acc -> x ^ 2 + acc) 0 [1 .. n]

{-1-}
within :: (Ord a) => [a] -> (a, a) -> [a]
within xs (a, b) = filter withinab xs
  where
    withinab x = a <= x && x <= b

{-2-}
sumrows :: (Num a) => [[a]] -> [a]
sumrows [] = []
sumrows xs = map sum xs

{-3-}
fact :: (Num a, Enum a) => a -> a
fact k = product [1 .. k]

approx :: (Fractional a, Enum a) => a -> a
approx n = sum (map (\a -> 1 / fact a) [0 .. n])

{-4-}
fingo :: [a] -> [a] -> [a]
-- the : operator prepend an element to a list such that when using foldr we get [1,2] [3,4] = [3,4,1,2]
fingo xs ys = foldr (:) xs ys

{-5-}
mapfunc :: [a -> b] -> [[a] -> [b]]
mapfunc = map map

{-a-}
partitionfilter :: (Num a) => (a -> Bool) -> [a] -> ([a], [a])
partitionfilter k xs = (filter k xs, filter (not . k) xs)

-- partitionfilter (==3) [3,4,3,3]
partitionfoldr :: (Eq a) => a -> [a] -> ([a], [a])
partitionfoldr k xs =
  foldr
    ( \x (yes, no) ->
        if x == k
          then (x : yes, no)
          else (yes, x : no)
    )
    ([], [])
    xs

{-b-}
filterusingfoldr :: (a -> Bool) -> [a] -> [a]
filterusingfoldr predicate xs = foldr (\x acc -> if predicate x then x : acc else acc) [] xs

{-c-}
remove :: String -> String -> String
remove [] ys = ys
remove xs ys = filter (`notElem` xs) ys

removefoldr :: String -> String -> String
removefoldr xs ys = foldr (\y acc -> if y `elem` xs then acc else y : acc) [] ys

{-d-}
{-
\**Recursive and not a higher order**
-- minimum xs finds the smallest element in xs
-- remove (minimum xs) xs removes the first occurence of the smallest value from xs
-- remove takes an minValue and a list and if the minValue is equal to head of the list it return the tail and effectively removes the first minValue
-- otherwise it keeps searching for the minValue
-- minimum (remove (minimum xs) xs) finds the smallest value after minValue has been removed
min2 :: (Ord a) => [a] -> a
min2 xs = minimum (remove (minimum xs) xs)
  where
    remove minValue (y : ys)
      | minValue == y = ys
      | otherwise = y : remove minValue ys
-}
-- takes an element a and a list xs, and returns the number of times a appears in xs.
count :: (Eq a) => a -> [a] -> Int
count a xs = length [x | x <- xs, a == x]

min2 :: (Ord a) => [a] -> a
min2 xs = if (count minValue xs > 1) then minValue else min2Value
  where
    minValue = foldr1 min xs
    -- returns a new list that includes all elements from xs that are not equal to minValue
    min2Value = foldr1 min (filter (/= minValue) xs)