-- Custom implementation of foldr for summing the elements of a list
foldrSum :: Num a => [a] -> a
foldrSum = foldr (\x acc -> x + acc) 0

-- Custom implementation of foldr for product of the elements in a list
foldrProduct :: Num a => [a] -> a
foldrProduct = foldr (\x acc -> x * acc) 1

-- Custom implementation of foldr to concatenate a list of strings
foldrConcat :: [String] -> String
foldrConcat = foldr (\x acc -> x ++ acc) ""

-- Custom implementation of foldr to calculate the length of a list
foldrLength :: [a] -> Int
foldrLength = foldr (\_ acc -> acc + 1) 0

-- Custom implementation of foldr to check if all elements satisfy a predicate
foldrAll :: (a -> Bool) -> [a] -> Bool
foldrAll p = foldr (\x acc -> p x && acc) True

-- Custom implementation of foldr to check if any element satisfies a predicate
foldrAny :: (a -> Bool) -> [a] -> Bool
foldrAny p = foldr (\x acc -> p x || acc) False

-- Custom implementation of foldr to filter elements based on a predicate
foldrFilter :: (a -> Bool) -> [a] -> [a]
foldrFilter p = foldr (\x acc -> if p x then x : acc else acc) []

-- Custom implementation of foldr to map a function over a list
foldrMap :: (a -> b) -> [a] -> [b]
foldrMap f = foldr (\x acc -> f x : acc) []

-- Custom implementation of foldr to calculate the maximum element in a list
foldrMaximum :: Ord a => [a] -> a
foldrMaximum = foldr1 (\x acc -> if x > acc then x else acc)

-- Custom implementation of foldr to calculate the minimum element in a list
foldrMinimum :: Ord a => [a] -> a
foldrMinimum = foldr1 (\x acc -> if x < acc then x else acc)

-- Custom implementation of foldr to reverse a list
foldrReverse :: [a] -> [a]
foldrReverse = foldr (\x acc -> acc ++ [x]) []

-- Custom implementation of foldr to find the last element in a list
foldrLast :: [a] -> Maybe a
foldrLast = foldr (\x _ -> Just x) Nothing

-- Custom implementation of foldr to zip two lists
foldrZip :: [a] -> [b] -> [(a, b)]
foldrZip xs ys = foldr (\(x, y) acc -> (x, y) : acc) [] (zip xs ys)

-- Custom implementation of foldr to flatten a list of lists
foldrConcatMap :: [[a]] -> [a]
foldrConcatMap = foldr (\x acc -> x ++ acc) []

-- Custom implementation of foldr to remove duplicates from a list
foldrNub :: Eq a => [a] -> [a]
foldrNub = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

foldrZipWithAdd :: Num a => [[a]] -> [a]
foldrZipWithAdd = foldr (zipWith (+)) (repeat 0)

-- Custom implementation of foldr to perform element-wise multiplication of lists
foldrZipWithMultiply :: Num a => [a] -> [a] -> [a]
foldrZipWithMultiply xs ys = foldr (\(x, y) acc -> (x * y) : acc) [] (zip xs ys)


-- Custom implementation of foldr to calculate the mean of a list of numbers
foldrMean :: Fractional a => [a] -> a
foldrMean xs = sum xs / fromIntegral (length xs)

-- Custom implementation of foldr to find the first element satisfying a predicate
foldrFind :: (a -> Bool) -> [a] -> Maybe a
foldrFind p = foldr (\x acc -> if p x then Just x else acc) Nothing

-- Custom implementation of foldr to calculate the factorial of a non-negative integer
foldrFactorial :: Integer -> Integer
foldrFactorial n = foldr (*) 1 [1..n]

-- Custom implementation of foldr to flatten a nested list
foldrFlatten :: [[a]] -> [a]
foldrFlatten = foldr (++) []

-- Custom implementation of foldr to calculate the sum of squares of odd numbers
foldrSumOfSquaresOfOdds :: Integral a => [a] -> a
foldrSumOfSquaresOfOdds = foldr (\x acc -> if odd x then x^2 + acc else acc) 0

-- Custom implementation of foldr to check if a list is a prefix of another list
foldrIsPrefixOf :: Eq a => [a] -> [a] -> Bool
foldrIsPrefixOf prefix xs = foldr (\(p, x) acc -> p == x && acc) (length prefix == length xs) (zip prefix xs)

-- Custom implementation of foldr to calculate the nth power of each element in a list
foldrNthPower :: Integral a => a -> [a] -> [a]
foldrNthPower n = foldr (\x acc -> x^n : acc) []

-- Custom implementation of foldr to check if a list is palindrome
foldrIsPalindrome :: Eq a => [a] -> Bool
foldrIsPalindrome xs = foldr (\(x, y) acc -> x == y && acc) True (zip xs (reverse xs))

-- Custom implementation of foldr to filter out elements greater than a threshold
foldrFilterGreaterThan :: Ord a => a -> [a] -> [a]
foldrFilterGreaterThan threshold = foldr (\x acc -> if x > threshold then x : acc else acc) []

-- Custom implementation of foldr to perform element-wise division of lists
foldrZipWithDivide :: Fractional a => [a] -> [a] -> [a]
foldrZipWithDivide xs ys = foldr (\(x, y) acc -> (x / y) : acc) [] (zip xs ys)

-- Custom implementation of foldr to find the first occurrence of a sublist in a list
foldrFindSublist :: (Foldable t, Eq a) => [a] -> t [a] -> Maybe [a]
foldrFindSublist sublist = foldr (\x acc -> if take (length sublist) x == sublist then Just x else acc) Nothing

-- Custom implementation of foldr to calculate the product of squares of even numbers
foldrProductOfSquaresOfEvens :: Integral a => [a] -> a
foldrProductOfSquaresOfEvens = foldr (\x acc -> if even x then x^2 * acc else acc) 1

-- Custom implementation of foldr to calculate the Fibonacci sequence up to a given index
foldrFibonacci :: Integer -> [Integer]
foldrFibonacci n = foldr (\_ acc@(x:y:_) -> x + y : acc) [1, 0] [1..n]

-- Custom implementation of foldr to check if a list is a suffix of another list
foldrIsSuffixOf :: Eq a => [a] -> [a] -> Bool
foldrIsSuffixOf suffix xs = foldr (\(s, x) acc -> s == x && acc) (length suffix == length xs) (zip suffix (reverse xs))

-- Custom implementation of foldr to flatten a tree represented as a list of lists
foldrFlattenTree :: [[a]] -> [a]
foldrFlattenTree = foldr (\x acc -> x ++ acc) []

-- Custom implementation of foldr to calculate the sum of cubes of odd numbers
foldrSumOfCubesOfOdds :: Integral a => [a] -> a
foldrSumOfCubesOfOdds = foldr (\x acc -> if odd x then x^3 + acc else acc) 0

-- Custom implementation of foldr to check if a list is a valid subsequence of another list
foldrIsValidSubsequence :: Eq a => [a] -> [a] -> Bool
foldrIsValidSubsequence sub xs = foldr (\(s, x) acc -> s == x && acc) (length sub == length xs) (zip sub xs)

-- Custom implementation of foldr to merge two sorted lists
foldrMergeSorted :: Ord a => [a] -> [a] -> [a]
foldrMergeSorted xs ys = foldr (\(x, y) acc -> if x <= y then x : acc else y : acc) [] (zip xs ys)

-- Custom implementation of foldr to calculate the sum of factorials up to a given index
foldrSumOfFactorials :: Integer -> Integer
foldrSumOfFactorials n = foldr (\x acc -> foldr (*) 1 [1..x] + acc) 0 [0..n]

-- Custom implementation of foldr to check if a list is palindrome when reversed
foldrIsPalindromeReverse :: Eq a => [a] -> Bool
foldrIsPalindromeReverse xs = foldr (\(x, y) acc -> x == y && acc) True (zip xs (reverse xs))

-- Custom implementation of foldr to find the last occurrence of an element in a list
foldrFindLast :: Eq a => a -> [a] -> Maybe a
foldrFindLast x xs = foldr (\(e, _) acc -> if e == x then Just x else acc) Nothing (zip xs [1..])

-- Custom implementation of foldr to flatten a nested list with varying levels of nesting
foldrFlattenNested :: [[a]] -> [a]
foldrFlattenNested = foldr (++) []

-- Custom implementation of foldr to calculate the product of cubes of even numbers
foldrProductOfCubesOfEvens :: Integral a => [a] -> a
foldrProductOfCubesOfEvens = foldr (\x acc -> if even x then x^3 * acc else acc) 1
