{-Prep 1-}
positions:: String -> [Int]
positions [] = []
positions (x:xs) = fromEnum x-96 : positions xs

{-Prep 2-}
sumsq:: Integral a => a -> a
sumsq a = sum [x^2 | x <- [0..a]]

sumsq' :: Integral a => a -> a
sumsq' n = foldr (\x acc -> x^2 + acc) 0 [1..n]

{-1-}
within :: Ord a => [a] -> (a, a) -> [a]
within xs (a,b) = filter withinab xs
    where 
        withinab x = a <= x && x <= b

{-2-}
sumrows::Num a => [[a]] -> [a]
sumrows [] = []
sumrows xs =  map sum xs

{-3-}
fact :: (Num a, Enum a) => a -> a
fact k = product [1..k]
approx :: (Fractional a, Enum a) => a -> a
approx n = sum(map (\a -> 1/fact a) [0..n])

{-4-}
fingo :: [a] -> [a] -> [a]
--the : operator prepend an element to a list such that when using foldr we get [1,2] [3,4] = [3,4,1,2]
fingo xs ys = foldr (:) xs ys

{-5-}
mapfunc :: [a -> b] -> [[a] -> [b]]
mapfunc = map map

{-a-}
partitionfilter::Num a => (a -> Bool) -> [a] -> ([a],[a])
partitionfilter k xs = (filter k xs, filter (not . k) xs)
--partitionfilter (==3) [3,4,3,3]
partitionfoldr::Eq a => a -> [a] -> ([a],[a])
partitionfoldr k xs = foldr (\x (yes,no) -> 
    if x == k 
        then (x:yes,no) 
    else (yes,x:no)) ([],[]) xs

{-b-}