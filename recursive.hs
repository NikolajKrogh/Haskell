-- Calculate the factorial of a non-negative integer
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Find the nth Fibonacci number
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Determine if a given integer is even
isEven :: Integer -> Bool
isEven 0 = True
isEven 1 = False
isEven n = isEven (n - 2)

-- Calculate the sum of squares up to a given positive integer
sumOfSquares :: Integer -> Integer
sumOfSquares 0 = 0
sumOfSquares n = n^2 + sumOfSquares (n - 1)

-- Reverse a list
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- Compute the nth triangular number
triangularNumber :: Integer -> Integer
triangularNumber 0 = 0
triangularNumber n = n + triangularNumber (n - 1)

-- Check if a list is a palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = x == last xs && isPalindrome (init xs)

-- Calculate the greatest common divisor (GCD) of two positive integers
gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

-- Calculate the sum of the first n natural numbers
sumOfNaturals :: Integer -> Integer
sumOfNaturals 0 = 0
sumOfNaturals n = n + sumOfNaturals (n - 1)

-- Compute the length of a list
listLength :: [a] -> Integer
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

-- Check if a given string is a palindrome
isStringPalindrome :: String -> Bool
isStringPalindrome "" = True
isStringPalindrome [_] = True
isStringPalindrome (x:xs) = x == last xs && isStringPalindrome (init xs)

-- Calculate the power of a number using exponentiation
power :: Integer -> Integer -> Integer
power _ 0 = 1
power x n = x * power x (n - 1)

-- Concatenate two lists
concatLists :: [a] -> [a] -> [a]
concatLists [] ys = ys
concatLists (x:xs) ys = x : concatLists xs ys

-- Compute the product of elements in a list
productOfList :: Num a => [a] -> a
productOfList [] = 1
productOfList (x:xs) = x * productOfList xs

-- Generate a list of the first n prime numbers
generatePrimes :: Integer -> [Integer]
generatePrimes n = take (fromIntegral n) (filter isPrime [2..])

-- Check if a number is prime
isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = not (any (\x -> n `mod` x == 0) [2..intSqrt n])

-- Integer square root (floor)
intSqrt :: Integer -> Integer
intSqrt = floor . sqrt . fromIntegral

-- Generate the first n terms of the Collatz sequence starting from a given number
collatzSequence :: Integer -> Integer -> [Integer]
collatzSequence 0 _ = []
collatzSequence n x = x : collatzSequence (n - 1) (if even x then x `div` 2 else 3 * x + 1)

-- Calculate the sum of squares of even numbers up to n
sumOfEvenSquares :: Integer -> Integer
sumOfEvenSquares 0 = 0
sumOfEvenSquares n
  | even n = n^2 + sumOfEvenSquares (n - 2)
  | otherwise = sumOfEvenSquares (n - 1)

-- Compute the nth triangular number using tail recursion
triangularNumberTail :: Integer -> Integer -> Integer
triangularNumberTail 0 acc = acc
triangularNumberTail n acc = triangularNumberTail (n - 1) (acc + n)

-- Generate a list of Fibonacci numbers up to n
fibonacciList :: Integer -> [Integer]
fibonacciList n = takeWhile (<= n) (map fibonacci [0..])

-- Check if a list is sorted in ascending order
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x1:x2:xs) = x1 <= x2 && isSorted (x2:xs)

-- Calculate the greatest common divisor (GCD) of a list of integers
gcdList :: [Integer] -> Integer
gcdList [] = 0  -- GCD is undefined for an empty list, handle this case accordingly
gcdList [x] = x
gcdList (x:xs) = gcd x (gcdList xs)

-- Double each element in a list
doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList (x:xs) = 2 * x : doubleList xs

-- Remove all occurrences of a specific element from a list
removeElement :: Eq a => a -> [a] -> [a]
removeElement _ [] = []
removeElement e (x:xs)
  | e == x = removeElement e xs
  | otherwise = x : removeElement e xs

-- Compute the sum of even Fibonacci numbers up to n
sumOfEvenFibonacci :: Integer -> Integer
sumOfEvenFibonacci n = sum (filter even (takeWhile (<= n) (map fibonacci [1..])))

-- Find the maximum element in a list
maximumList :: Ord a => [a] -> a
maximumList [x] = x
maximumList (x:xs) = max x (maximumList xs)

-- Merge two sorted lists into a single sorted list
mergeSortedLists :: Ord a => [a] -> [a] -> [a]
mergeSortedLists xs [] = xs
mergeSortedLists [] ys = ys
mergeSortedLists (x:xs) (y:ys)
  | x <= y = x : mergeSortedLists xs (y:ys)
  | otherwise = y : mergeSortedLists (x:xs) ys

-- Calculate the product of the first n prime numbers
productOfPrimes :: Integer -> Integer
productOfPrimes n = product (take (fromIntegral n) (filter isPrime [2..]))

-- Check if a list contains consecutive duplicates
hasConsecutiveDuplicates :: Eq a => [a] -> Bool
hasConsecutiveDuplicates [] = False
hasConsecutiveDuplicates [_] = False
hasConsecutiveDuplicates (x1:x2:xs) = x1 == x2 || hasConsecutiveDuplicates (x2:xs)

-- Generate a list of powers of 2 up to n
powersOf2 :: Integer -> [Integer]
powersOf2 n = map (2^) [0..n]

-- Flatten a list of lists into a single list
flatten :: [[a]] -> [a]
flatten [] = []
flatten (xs:xss) = xs ++ flatten xss

-- Calculate the sum of digits of a positive integer
sumOfDigits :: Integer -> Integer
sumOfDigits n
  | n < 10 = n
  | otherwise = n `mod` 10 + sumOfDigits (n `div` 10)

-- Determine if a list is a subset of another list
isSubset :: Eq a => [a] -> [a] -> Bool
isSubset [] _ = True
isSubset (x:xs) ys = elem x ys && isSubset xs ys

-- Count occurrences of a specific element in a list
countOccurrences :: Eq a => a -> [a] -> Integer
countOccurrences _ [] = 0
countOccurrences x (y:ys) = (if x == y then 1 else 0) + countOccurrences x ys

-- Generate a list of the first n perfect squares
perfectSquares :: Integer -> [Integer]
perfectSquares n = take (fromIntegral n) (map (\x -> x * x) [1..])

-- Compute the nth power of a number using exponentiation
nthPower :: Integer -> Integer -> Integer
nthPower _ 0 = 1
nthPower x n = x * nthPower x (n - 1)

-- Merge two sorted lists into a single sorted list
mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted xs [] = xs
mergeSorted [] ys = ys
mergeSorted (x:xs) (y:ys)
  | x <= y    = x : mergeSorted xs (y:ys)
  | otherwise = y : mergeSorted (x:xs) ys

-- Generate a list of the first n triangular numbers
triangularNumbers :: Integer -> [Integer]
triangularNumbers n = take (fromIntegral n) (scanl1 (+) [1..])

-- Reverse the order of words in a string
reverseWords :: String -> String
reverseWords = unwords . reverse . words

-- Replace all occurrences of a character in a string
replaceChar :: Char -> Char -> String -> String
replaceChar _ _ [] = []
replaceChar oldChar newChar (x:xs)
  | x == oldChar = newChar : replaceChar oldChar newChar xs
  | otherwise    = x : replaceChar oldChar newChar xs

-- Check if a list is a subsequence of another list
isSubsequence :: Eq a => [a] -> [a] -> Bool
isSubsequence [] _ = True
isSubsequence _ [] = False
isSubsequence (x:xs) (y:ys)
  | x == y    = isSubsequence xs ys
  | otherwise = isSubsequence (x:xs) ys

-- Generate a list of the first n terms of the hailstone sequence starting from a given number
hailstoneSequence :: Integer -> Integer -> [Integer]
hailstoneSequence 0 _ = []
hailstoneSequence n x = x : hailstoneSequence (n - 1) (if even x then x `div` 2 else 3 * x + 1)

-- Calculate the product of even numbers in a list
productOfEvens :: Integral a => [a] -> a
productOfEvens [] = 1
productOfEvens (x:xs)
  | even x    = x * productOfEvens xs
  | otherwise = productOfEvens xs

-- Remove all occurrences of a character from a string
removeChar :: Char -> String -> String
removeChar _ [] = []
removeChar c (x:xs)
  | c == x    = removeChar c xs
  | otherwise = x : removeChar c xs

-- Find the minimum element in a list
findMinimum :: Ord a => [a] -> a
findMinimum [x] = x
findMinimum (x:xs) = min x (findMinimum xs)

-- Calculate the sum of even-indexed elements in a list
sumOfEvenIndices :: Num a => [a] -> a
sumOfEvenIndices xs = sum (map snd (filter (\(i, _) -> even i) (zip [0..] xs)))

-- Returns the sum from a given number down to 0
sumdown:: Int -> Int 
sumdown 0 = 0
sumdown x = x + sumdown (x-1) 

-- Returns the exponentiation of a number
expo :: Int -> Int -> Int
expo _ 0 = 1
expo x y = x * expo x (y-1)

-- Calculating the greatest common divisor of two numbers
euclid :: Int -> Int -> Int
euclid x y
  | x == y = x
  | x > y = euclid (x-y) y
  | x < y = euclid x (y-x)

-- Returns the length of a list
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- Merge two sorted lists into a single sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys

-- Split a list into two halves (first half is longer if the length is odd)
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- Merge sort
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where (left, right) = halve xs