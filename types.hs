-- Basic Operations

-- Maximum of Two Numbers
myMax :: (Ord a) => a -> a -> a
myMax x y | x >= y = x | otherwise = y

-- Minimum of Two Numbers
myMin :: (Ord a) => a -> a -> a
myMin x y | x <= y = x | otherwise = y

-- Absolute Value
myAbs :: (Ord a, Num a) => a -> a
myAbs x | x >= 0 = x | otherwise = -x

-- Signum
signum :: (Ord a1, Num a1, Num a2) => a1 -> a2
signum x | x > 0 = 1 | x < 0 = -1 | otherwise = 0

-- Is Even
isEven :: (Integral a) => a -> Bool
isEven n = n `mod` 2 == 0

-- Is Odd
isOdd :: (Integral a) => a -> Bool
isOdd n = not (isEven n)

-- List Operations

-- Reverse a List
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- Append Two Lists
append :: [a] -> [a] -> [a]
append xs ys = xs ++ ys

-- Find the Maximum Element in a List
myMaximum :: (Ord a) => [a] -> a
myMaximum [x] = x
myMaximum (x : xs) = max x (myMaximum xs)

-- Find the Minimum Element in a List
myMinimum :: (Ord a) => [a] -> a
myMinimum [x] = x
myMinimum (x : xs) = min x (myMinimum xs)

-- Sum of a List
mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

-- Product of a List
myProduct :: (Num a) => [a] -> a
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

-- Length of a List
myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

-- Take the First N Elements of a List
myTake :: (Num t, Ord t) => t -> [a] -> [a]
myTake 0 _ = []
myTake n xs = takeHelper n xs
  where
    takeHelper n [] = []
    takeHelper n (x : xs)
      | n > 0 = x : takeHelper (n - 1) xs
      | otherwise = []

-- Drop the First N Elements of a List
myDrop :: (Num t, Ord t) => t -> [a] -> [a]
myDrop 0 xs = xs
myDrop n xs = dropHelper n xs
  where
    dropHelper n [] = []
    dropHelper n (_ : xs)
      | n > 0 = dropHelper (n - 1) xs
      | otherwise = xs

-- Concatenate Two Lists
concatenate :: [a] -> [a] -> [a]
concatenate xs ys = xs ++ ys

-- String Operations

-- Check if a String is Empty
isEmpty :: String -> Bool
isEmpty "" = True
isEmpty _ = False

-- Join a List of Strings with a Separator
join :: (Foldable t) => [Char] -> t [Char] -> [Char]
join sep strs = foldr (\s acc -> s ++ sep ++ acc) "" strs

-- Remove Leading and Trailing Whitespace
-- trim :: String -> String
trim :: [Char] -> [Char]
trim xs = reverse (dropWhile isMySpace (reverse xs))
  where
    isMySpace c = c == ' ' || c == '\n' || c == '\t' || c == '\r'

-- Find the Length of a String
-- stringLength :: String -> Int
stringLength :: (Foldable t, Num b, Num (b -> b)) => t a -> b
stringLength xs = foldr (\_ -> 1 + 0) 0 xs

-- Math Operations

factorial :: (Eq t, Num t) => t -> t
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- implementation of the Euclidean algorithm, which is used to calculate the greatest common divisor (GCD)
myGcd :: (Integral a) => a -> a -> a
myGcd a b
  | b == 0 = a
  | otherwise = myGcd b (a `mod` b)

-- Least common multiplier (LCM)
lcm :: (Integral a) => a -> a -> a
lcm a b = abs (a * b) `div` gcd a b

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

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

-- Logical Operations

myNot :: Bool -> Bool
myNot True = False
myNot False = True

myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd _ _ = False

myOr :: Bool -> Bool -> Bool
myOr True _ = True
myOr _ True = True
myOr False False = False

-- List Comprehensions

squareList :: (Num a) => [a] -> [a]
squareList xs = [x * x | x <- xs]

filterEven :: (Integral a) => [a] -> [a]
filterEven xs = [x | x <- xs, isEven x]

-- Higher-Order Functions

-- myMap :: (a -> b) -> [a] -> [b]
myMap :: (t -> a) -> [t] -> [a]
myMap f xs = [f x | x <- xs]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = [x | x <- xs, p x]

-- myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
myFoldr f b xs = case xs of
  [] -> b
  (x : xs) -> f x (myFoldr f b xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f b xs = case xs of
  [] -> b
  (x : xs) -> myFoldl f (f b x) xs

-- Tuples

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Basic Operations

-- Sum of first and second element
firstTwoSum :: (Num a) => [a] -> a
firstTwoSum (x : y : _) = x + y
firstTwoSum _ = 0

-- Length of a string excluding spaces
trimmedLength :: [Char] -> Int
trimmedLength xs = length (filter (/= ' ') xs)

-- Check if all elements are even
allEven :: (Integral a) => [a] -> Bool
allEven [] = True
allEven (x : xs) = even x && allEven xs

-- List of characters from a string in reverse order
reversedChars :: [a] -> [a]
reversedChars xs = reverse (map (\c -> c) xs)

-- List of absolute values
absList :: (Num b) => [b] -> [b]
absList xs = map abs xs

-- List of squared values
squaredList :: (Num b) => [b] -> [b]
squaredList xs = map (^ 2) xs

-- Check if a string starts with a specific character
startsWith :: (Eq a) => a -> [a] -> Bool
startsWith c xs = head xs == c

-- Find the maximum value in a list
maxValue :: (Ord a) => [a] -> Maybe a
maxValue [] = Nothing
maxValue (x : xs) = Just (maximum (x : xs))

-- Filter elements greater than a given value
filterGreaterThan :: (Ord a) => a -> [a] -> [a]
filterGreaterThan n xs = filter (> n) xs

-- Remove the first element from a list
tailList :: [a] -> [a]
tailList (x : xs) = xs
tailList _ = []

-- Check if a number is divisible by 3
isDivisibleByThree :: (Integral a) => a -> Bool
isDivisibleByThree n = mod n 3 == 0

-- Multiply all elements in a list
productOfList :: (Num a) => [a] -> a
productOfList [] = 1
productOfList (x : xs) = x * productOfList xs

-- Check if a string is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Basic operations

-- Combine first and last element of a list
firstLast :: (Show a) => [a] -> String
firstLast [x, y] = "First: " ++ show x ++ ", Last: " ++ show y
firstLast _ = "Empty list"

-- Reverse a string without recursion
reverseString :: String -> String
reverseString xs = foldl (\acc c -> c : acc) "" xs

-- Count the occurrences of an element in a list
countElement :: (Eq a) => a -> [a] -> Int
countElement x xs = length (filter (== x) xs)

-- String operations

-- Check if a string starts and ends with the same character
isFramed :: Char -> String -> Bool
isFramed c xs = head xs == c && last xs == c

-- List operations

-- Check if a list is sorted in ascending order
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : xs) = x <= y && isSorted (y : xs)

-- Logic operations

-- Check if a number is positive
isPositive :: (Ord a, Num a) => a -> Bool
isPositive n = n > 0

-- Implement exclusive OR (XOR) for two Booleans
xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

-- Data structures

-- The nub function in Haskell removes duplicate elements from a list.
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub (filter (/= x) xs)

-- Create a new list with duplicate elements removed
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates xs = nub xs

-- Create a dictionary from a list of key-value pairs
createDictionary :: p -> p
createDictionary xs = xs

-- Functions

-- Compose two functions
compose :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
compose f g = \x -> f (g x)

isAlwaysFalse :: (Bool -> Bool) -> Bool
isAlwaysFalse f = all (not . f) [True, False]

-- Check if a function is commutative
isCommutative :: (Eq a) => (t -> t -> a) -> t -> t -> Bool
isCommutative f x y = f x y == f y x

-- Basic operations

-- Check if a list contains at least one element
hasElement :: [a] -> Bool
hasElement xs = not (null xs)

-- String operations

-- Find the longest common prefix of two strings
longestPrefix :: (Eq a) => [a] -> [a] -> [a]
longestPrefix (x : xs) (y : ys)
  | x == y = x : longestPrefix xs ys
  | otherwise = []
longestPrefix _ _ = []

-- List operations

-- Filter out elements that are not unique
uniqueElements :: (Eq a) => [a] -> [a]
uniqueElements xs = nub xs

-- Partition a list based on a predicate
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

-- Check if a list is a permutation of another list
isPermutation :: (Eq a) => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && all (`elem` ys) xs

-- Mathematical operations

-- Calculate the absolute difference between two numbers
absDiff :: (Num a) => a -> a -> a
absDiff x y = abs (x - y)

-- Check if a number is a perfect square
isPerfectSquare :: (Integral a, Floating a) => a -> Bool
isPerfectSquare n = sqrt n `mod` 1 == 0

-- Logic operations

-- Implement NAND (NOT AND) for two Booleans
nand :: Bool -> Bool -> Bool
nand a b = not (a && b)

-- Check if all elements in a list satisfy a predicate
allSatisfy :: (a -> Bool) -> [a] -> Bool
allSatisfy p xs = all p xs

-- Data structures

-- Create a new list with elements in reverse order
reverseList :: (Show a) => [a] -> [a]
reverseList xs = reverse xs

-- Insert an element at a specific index in a list
insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs
  | n < 0 || n > length xs = xs
  | otherwise = take n xs ++ [x] ++ drop n xs

-- Functions

-- Apply a function to each element and collect the results in a list
mapCollect :: (a -> [b]) -> [a] -> [[b]]
mapCollect f xs = map f xs

-- Basic operations

-- Group elements in a list based on a predicate eg. groupBy (==) [1,2,2,3,3,3] = [[1],[2,2],[3,3,3]]
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy eq (x : xs) = (x : ys) : groupBy eq zs
  where
    (ys, zs) = span (eq x) xs

-- Check if a list is a palindrome using recursion ([a])
isPalindromeRec :: (Eq a) => [a] -> Bool
isPalindromeRec [] = True
isPalindromeRec [_] = True
isPalindromeRec (x : xs) = x == last xs && isPalindromeRec (takeWhile (/= x) xs)

-- String operations

-- Split a string into words based on whitespace (String)
splitWords :: String -> [String]
splitWords xs = words (filter (/= ' ') xs)

-- List operations

-- Remove consecutive duplicates from a list (Eq)
removeConsecDuplicates :: (Eq a) => [a] -> [a]
removeConsecDuplicates xs = let (ys, zs) = span (== head xs) xs in ys ++ dropWhile (== head xs) zs

-- Check if a list is strictly increasing (Ord)
isStrictlyIncreasing :: (Ord a) => [a] -> Bool
isStrictlyIncreasing [] = True
isStrictlyIncreasing [_] = True
isStrictlyIncreasing (x : y : xs) = x < y && isStrictlyIncreasing (y : xs)

-- Mathematical operations

-- Calculate the nth Fibonacci number (Integral)
fib :: (Integral a) => Int -> a
fib n
  | n < 0 = error "fib: negative index"
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)

-- Check if a number is prime (Integral)
isPrime :: (Integral a, Floating a) => a -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = all (not . (\d -> n `mod` d == 0)) [2 .. sqrt n]

-- Logic operations

-- Implement NOR (NOT OR) for two Booleans
nor :: Bool -> Bool -> Bool
nor a b = not (a || b)

-- Check if any element in a list satisfies a predicate (Bool)
anySatisfy :: (a -> Bool) -> [a] -> Bool
anySatisfy p xs = any p xs

-- Functions

-- Compose three functions (f: c -> d, g: b -> c, h: a -> b)
compose3 :: (c -> d) -> (b -> c) -> (a -> b) -> (a -> d)
compose3 f g h = \x -> f (g (h x))

-- Check if a function is associative (a -> b -> c)
isAssociative :: (Eq t) => (t -> t -> t) -> t -> t -> t -> Bool
isAssociative f x y z = f (f x y) z == f x (f y z)

a :: (a -> b) -> a -> b
a f x = f x

b :: (t -> a -> a) -> a -> a
b f d = f (error "First argument") $! f (error "Second argument") d

bb :: [(Integer, Char)]
bb = zip [1, 2, 3] ['a', 'b', 'c']

bbb :: ([Integer], [Char])
bbb = unzip [(1, 'a'), (2, 'b'), (3, 'c')]

ddd :: ([Integer], [Integer])
ddd = splitAt 2 [1, 2, 3, 4, 5]

eeeeee :: Maybe Integer
eeeeee = lookup "a" [("a", 1), ("b", 2)]

hhhh :: IO [()]
hhhh = mapM print [1, 2, 3]

hhhhh :: Maybe [Integer]
hhhhh = sequence [Just 1, Just 2, Just 3]

hhhhhh :: Integer
hhhhhh = foldr1 (+) [1, 2, 3, 4, 5]

oo :: Integer
oo = foldl (+) 0 [1, 2, 3, 4, 5]

ooo :: [a] -> [b] -> [(a, b)]
ooo = zip

-- This case involves parametric polymorphism since it's polymorphic over any type a that is an instance of both Eq and Show.
-- exercise1 :: Foldable t => p -> [a1] -> [t a2] -> [(a1, Int)]
exercise1 xs = zipWith (\x ys -> (x, length ys))

-- This case doesn't involve polymorphism. It's a monomorphic function that operates on strings and characters.
exercise2 :: String -> Char -> Int -> String
exercise2 str ch n = replicate n ch ++ str

-- This case involves parametric polymorphism since it's polymorphic over any type a that is an instance of both Ord and Num.
-- exercise3 :: (Ord a, Num a) => a -> a -> a -> Bool
exercise3 x y z = x + y > z

-- This case involves ad hoc polymorphism since it's polymorphic over any type a that is an instance of Show.
-- exercise6 :: (Show a) => a -> String
exercise6 x = show x

-- This case involves parametric polymorphism since it's polymorphic over any type a that is an instance of Num.
-- exercise9 :: (Num a) => a -> a -> a
exercise9 x y = x + y

-- ch :: Num a => Char -> a
ch c = if c == 'a' then 1 else 0

-- This case doesn't involve polymorphism. It's a monomorphic function that performs logical AND operation.
-- exercise11 :: Bool -> Bool -> Bool
exercise11 True True = True
exercise11 _ _ = False

-- This case involves parametric polymorphism since it's polymorphic over lists of any type a that is an instance of Num.
-- exercise15 :: (Num a) => [a] -> a
exercise15 xs = sum [x | x <- xs]

-- exercise16 :: (Foldable t, Num a) => t a -> a
exercise16 xs = sum xs

-- This case involves ad hoc polymorphism and parametric since it's polymorphic over any type a that is an instance of Num or Eq.
-- exercise18 :: (Eq a, Num a) => ((a -> Bool) -> t) -> t
exercise18 x = x (== 0)

-- This case involves parametric polymorphism since it's polymorphic over lists of any type a that is an instance of Eq.
-- exercise20 :: Eq a => p -> a -> a -> Bool
exercise20 xs = (==)

-- This case involves parametric polymorphism with Foldable and Num
-- exercise21 :: (Foldable t, Ord a) => t a -> a
exercise21 xs = maximum xs

-- This case involves ad hoc polymorphism with two type classes: Num and Show.
-- exercise22 :: (Show a, Num a) => a -> String
exercise22 x = show (x + 10)

-- This case involves parametric polymorphism
-- exercise23 :: Enum a => a -> a -> [a]
exercise23 start end = [start .. end]

-- This case involves parametric polymorphism
-- exercise24 :: Ord a => a -> a -> b -> (a, b)
exercise24 x y z = if x < y then (y, z) else (x, z)

-- This case involves parametric polymorphism
-- exercise25 :: Eq a => a -> a -> Bool
exercise25 x y = x /= y

-- This case involves ad hoc polymorphism with three type classes: Ord, Foldable and Show
-- exercise26 :: (Show a, Foldable t, Ord a) => t a -> [Char]
exercise26 xs = "Minimum: " ++ show (minimum xs) ++ ", Maximum: " ++ show (maximum xs)

-- This case involves ad hoc polymorphism with Num b
-- exercise27 :: Num b => [b] -> [(b, b)]
exercise27 xs = zip xs (map (+ 1) xs)

-- This case involves parametric polymorphism with three type classes: Foldable, Num and Ord
-- exercise28 :: (Foldable t, Num a, Ord a) => t a -> a
exercise28 xs = if null xs then 0 else maximum xs - minimum xs

-- This case involves parametric polymorphism
-- exercise29 :: Num a => a -> a -> [a]
exercise29 x y = [x, y, x + y]

-- This case involves parametric polymorphism with four type classes: Eq, Ord, Num, and Show.
-- exercise30 :: (Ord a, Num a) => [a] -> [(a, Bool)]
exercise30 xs = [(x, x > 0) | x <- xs]

-- Explanation: This function add is polymorphic in the sense that it works for any numeric type a that is an instance of the Num type class.
-- It demonstrates ad-hoc polymorphism.
add :: (Num a) => a -> a -> a
add x y = x + y

-- Explanation: The head' function is polymorphic and can work with lists of any type a. It demonstrates parametric polymorphism.
head' :: [a] -> a
head' (x : _) = x
head' [] = error "Empty list!"

-- Explanation: The maximum' function finds the maximum element in a list of any ordered type a. It shows parametric polymorphism.
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list!"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

-- Explanation: The function isVowel determines whether a character is a vowel or not. It demonstrates ad-hoc polymorphism.
isVowel :: Char -> Bool
isVowel c = c `elem` "aeiouAEIOU"

-- Explanation: The toMaybe function wraps any value of type a into a Maybe type. It exhibits parametric polymorphism.
toMaybe :: a -> Maybe a
toMaybe x = Just x

-- Explanation: The toString function converts any value to a string representation using the show function.
-- It demonstrates ad-hoc polymorphism.
toString :: (Show a) => a -> String
toString x = "The value is: " ++ show x

-- Explanation: The productList function computes the product of elements in a list of any numeric type a.
-- It demonstrates ad-hoc polymorphism.
productList :: (Num a) => [a] -> a
productList [] = 1
productList (x : xs) = x * productList xs

-- Explanation: The isEqual function checks whether two values of any equatable type a are equal.
-- It illustrates parametric polymorphism.
isEqual :: (Eq a) => a -> a -> Bool
isEqual x y = x == y
