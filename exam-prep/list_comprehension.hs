-- Calculates the sum of the squares of the first 100 natural numbers
squaresSum :: Integer
squaresSum = sum [x ^ 2 | x <- [1 .. 100]]

-- Calculates all possible grid combinations
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

-- Calculates all possible grid combinations excluding all diagonal points (x,y) where x == y
grid' :: Int -> [(Int, Int)]
grid' n = [(x, y) | x <- [0 .. n], y <- [0 .. n], x /= y]

-- Replicates a value n times
replicate' :: Int -> a -> [a]
replicate' x n = [n | _ <- [1 .. x]]

-- Pythagorean triples
pyths :: Int -> [(Int, Int, Int)]
pyths k = [(x, y, z) | x <- [1 .. k], y <- [1 .. k], z <- [1 .. k], x ^ 2 + y ^ 2 == z ^ 2]

-- Perfect number
factors :: Integral a => a -> [a]
factors n = [x | x <- [1 .. n - 1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects x = [a | a <- [1 .. x], isPerfect a]
  where
    isPerfect num = num == sum (factors num)

-- Redefine the function positions using the function find
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (m, v) <- t, m == k]

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (zip xs [0 ..])

-- Scalar product
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- Caesar cipher that can handle both upper and lower case letters
isLower :: Char -> Bool
isLower c = c >= 'a' && c <= 'z'

isUpper :: Char -> Bool
isUpper c = c >= 'A' && c <= 'Z'

toLower :: Char -> Char
toLower c
  | isUpper c = toEnum (fromEnum c + 32)
  | otherwise = c

toUpper :: Char -> Char
toUpper c
  | isLower c = toEnum (fromEnum c - 32)
  | otherwise = c

let2int :: Char -> Int
let2int c = fromEnum c - fromEnum 'a'

int2let :: Int -> Char
int2let n = toEnum (fromEnum 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = toUpper (int2let ((let2int (toLower c) + n) `mod` 26))
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- Define the function concat' using list comprehension
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

-- Return all primes up to n
primes :: Int -> [Int]
primes n = sieve [2 .. n]
  where
    sieve [] = []
    sieve (p : ps) = p : sieve [x | x <- ps, mod x p /= 0]

-- Picks out all occurrence of an integer k
matches :: Integer -> [Integer] -> [Integer]
matches k xs = [x | x <- xs, x == k]

-- Checks if element exists in the list 
elem' :: Integer -> [Integer] -> Bool
elem' k xs = length [x | x <- xs, x == k] > 0