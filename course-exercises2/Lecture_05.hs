-- Exercise 1
rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]

-- Exercise 2
myLast :: [a] -> a
myLast [] = error "List cannot be empty"
myLast xs = head (reverse xs)

-- Exercise 3
myisolate :: (Eq a) => [a] -> a -> ([a], [a])
myisolate xs element = (filter (/= element) xs, filter (== element) xs)

-- Exercise 4
wrapup :: (Eq a) => [a] -> [[a]]
wrapup [] = []
wrapup (x : xs) = (x : takeWhile (== x) xs) : wrapup (dropWhile (== x) xs)

-- Exercise 5
triples :: (Num a) => [(a, a, a)] -> ([a], [a], [a])
triples [(a, b, c)] = ([a], [b], [c])
triples ((x, y, z) : xs) = (x : xs', y : ys', z : zs')
  where
    (xs', ys', zs') = triples xs

-- a)
-- run length encoding
rle :: (Eq a) => [a] -> [(a, Int)]
rle [] = []
rle (x : xs) = (x, countElement) : rle remainingAfterRemoval
  where
    countElement = length (x : takeWhile (== x) xs)
    remainingAfterRemoval = dropWhile (== x) xs

-- b)
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs) = f x || myAny f xs

-- c)
frequencies :: String -> [(Char, Int)]
frequencies [] = []
frequencies (x : xs) = (x, countElement) : frequencies remainingElements
  where
    countElement = 1 + length (filter (== x) xs)
    remainingElements = [remove | remove <- xs, remove /= x]
