-- Exercise 1
sevens :: Int -> [Int]
sevens n = [x | x <- [1 .. n], x `mod` 7 == 0]

-- Exercise 2
pyt :: Int -> [(Int, Int, Int)]
pyt n = [(a, b, c) | a <- [1 .. n], b <- [1 .. n], c <- [1 .. n], a ^ 2 + b ^ 2 == c ^ 2]

-- Exercise 3
headsup :: Eq a => [a] -> Bool
headsup x = if head x == head (tail x) then True else False

-- Exercise 4
plonk :: Num a => a -> a -> a -> a
plonk x y z = x + y + z

-- Exercise 5
-- findtype::(Ord a1, Eq a2) => a2 -> a2 -> (a1, a1)-> a1
findtype x y (a, b) = if x == y || x >= y then a else b

-- a)
flop :: [(a, b)] -> [(b, a)]
flop xs = [(x, y) | (y, x) <- xs]

-- b)
dupli :: [a] -> [a]
dupli xs = [x | x <- xs, _ <- [1, 2]]

-- c)
isperfect :: Int -> Bool
isperfect n = n == sum [x | x <- [1 .. n - 1], n `mod` x == 0]

-- d)
bighead :: [Int] -> Int
bighead xs = length [x | x <- xs, x > head xs]


--sums m n = [x + y | x <- [1 .. m], y <- [1 .. n]]

sums :: (Num a, Enum a) => a -> a -> [a]
sums m n = concat [[x+y | x <- [1..m]] | y <- [1..n]]