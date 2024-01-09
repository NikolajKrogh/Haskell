charlist = ['a' .. 'z']

intlist = [0 .. 99]

palindromelist = [1, 2, 3, 2, 1]

-- 1. Find the last element of a list.
lastelem :: [a] -> a
lastelem xs = last xs

myLast :: [a] -> a
myLast [x] = x
myLast (_ : xs) = myLast xs

myLast' :: [a] -> Maybe a
myLast' [] = Nothing
myLast' xs = Just (foldr1 (const id) xs)

myLast'' :: [a] -> Maybe a
myLast'' [] = Nothing
myLast'' xs = Just (foldr1 (flip const) xs)

myLast''' = head . reverse

myLast'''' :: [a] -> Maybe a
myLast'''' [] = Nothing
-- If the input list is not empty (xs), the function uses foldl1 to reduce the list to its last element.
-- curry snd is a binary function that ignores its first argument and returns its second argument. When used with foldl1, it effectively returns the last element of the list.
myLast'''' xs = Just (foldl1 (curry snd) xs)

myLast''''' :: [a] -> a
myLast''''' x = x !! (length x - 1)

-- 2. Find the second last element of a list
-- myButLast :: [a] -> a
myButLast xs = xs !! (length xs - 2)

myButLast' x = reverse x !! 1

myButLast'' = last . init

-- 3. find the K'th element of a list
elementAt :: [a] -> Int -> a
elementAt xs n = xs !! (n - 1)

elementAt' :: [a] -> Int -> a
elementAt' (x : _) 1 = x
elementAt' (_ : xs) i = elementAt' xs (i - 1)

-- 4. Find the number of elements in a list
myLength :: [a] -> Int
myLength xs = length xs

myLength' :: [a] -> Int
myLength' [] = 0
myLength' (_ : xs) = 1 + myLength' xs

-- 5. Reverse a list
myReverse :: [a] -> [a]
myReverse = foldr (\x acc -> acc ++ [x]) []

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

myReverse'' :: [a] -> [a]
myReverse'' [] = []
myReverse'' (x : xs) = myReverse'' xs ++ [x]

-- 6. Find out wheter a list is a palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' xs = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)

-- 7. Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List (x : xs)) = myFlatten x ++ myFlatten (List xs)
myFlatten (List []) = []

flattenlist = myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])

myFlatten' :: NestedList a -> [a]
myFlatten' (Elem x) = [x]
myFlatten' (List x) = concatMap myFlatten' x

-- 8. Eliminate consecutive duplicates of list elements
myCompress :: Eq a => [a] -> [a]
myCompress [] = []
myCompress [x] = [x]
myCompress (x : y : xs)
  | x == y = myCompress (y : xs)
  | otherwise = x : myCompress (y : xs)

myCompress' [] = []
myCompress' (x : xs) = x : myCompress' (dropWhile (== x) xs)

-- 9. Pack consecutive duplicates of list elements into sublists.
packlist = ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']

myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack [x] = [[x]]
-- If the input list has at least one element (x:xs), it checks if x is an element of the first sublist of the packed version of xs.
-- If so, it adds x to that sublist and keeps the rest of the packed list the same.
-- If not, it adds a new sublist containing x to the front of the packed list.
myPack (x : xs) =
  if x `elem` (head (myPack xs))
    then (x : (head (myPack xs))) : (tail (myPack xs))
    else [x] : (myPack xs)

myPack' :: (Eq a) => [a] -> [[a]]
myPack' [] = []
-- It creates a new sublist that starts with x and includes all the consecutive elements that are equal to x.
-- This new sublist is then prepended to the result of recursively packing the rest of the list, which is obtained by dropping all elements equal to x from xs using dropWhile (==x) xs.
myPack' (x : xs) = (x : takeWhile (== x) xs) : myPack' (dropWhile (== x) xs)

-- 10. Run-length encoding of list using problem 9

-- Higher order (Map)
myEncode :: Eq a => [a] -> [(Int, a)]
myEncode xs = map (\x -> (length x, head x)) (myPack xs)

-- List comprehension
myEncode' :: Eq a => [a] -> [(Int, a)]
myEncode' xs = [(length x, head x) | x <- myPack xs]

-- List comprehension
myEncode'' :: Eq a => [a] -> [(Int, a)]
myEncode'' xs = [(length (x : xs), x) | (x : xs) <- myPack xs]

-- Higher order (foldr)
myEncode''' :: Eq a => [a] -> [(Int, a)]
myEncode''' xs = (enc . myPack) xs
  where
    enc = foldr (\x acc -> (length x, head x) : acc) []
