-- Exercise 1
rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]

-- Exercise 2
myLast :: [a] -> a
myLast [] = error "List cannot be empty"
myLast xs = head (reverse xs)

-- Exercise 3
myisolate :: Eq a => [a] -> a -> ([a], [a])
myisolate xs element = (filter (/= element) xs, filter (== element) xs)

-- Exercise 4
wrapup :: Eq a => [a] -> [[a]]
wrapup [] = []
wrapup (x : xs) = (x : takeWhile (== x) xs) : wrapup (dropWhile (== x) xs)

-- Exercise 5
triples :: Num a => [(a, a, a)] -> ([a], [a], [a])
triples [(a, b, c)] = ([a], [b], [c])

triples ((x,y,z):xs) = (x:xs', y:ys', z:zs')
    where (xs', ys', zs') = triples xs