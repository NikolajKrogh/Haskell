-- 11. Modified run-length encoding (Modify problem 10 so that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.)
encodelist = ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']

data ListItem a = Single a | Multiple Int a deriving (Show)

myPack' :: (Eq a) => [a] -> [[a]]
myPack' [] = []
myPack' (x : xs) = (x : takeWhile (== x) xs) : myPack' (dropWhile (== x) xs)

myEncode :: Eq a => [a] -> [(Int, a)]
myEncode xs = map (\x -> (length x, head x)) (myPack' xs)

myEncodeModified :: Eq a => [a] -> [ListItem a]
myEncodeModified = map encodeHelper . myEncode
  where
    encodeHelper (1, x) = Single x
    encodeHelper (n, x) = Multiple n x

-- 12. Decode a run-length encoded list
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
encodedlist1 = [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']

encodedlist2 :: [(Int, Char)]
encodedlist2 = [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')]

myDecodeModified :: [ListItem a] -> [a]
myDecodeModified = concatMap decodeHelper
  where
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple n x) = replicate n x

myDecode' :: [(Int, a)] -> [a]
myDecode' = concatMap (uncurry replicate)

-- 13. Run-length encoding of a list (direct solution)
-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

myEncodeDirect' :: Eq a => [a] -> [ListItem a]
myEncodeDirect' = map encodeHelper . myEncode
  where
    encodeHelper (1, x) = Single x
    encodeHelper (n, x) = Multiple n x

myEncodeDirect :: Eq a => [a] -> [ListItem a]
myEncodeDirect [] = []
myEncodeDirect (x : xs) = encodeHelper 1 x xs
  where
    encodeHelper n x [] = [encodeHelper' n x]
    encodeHelper n x (y : ys)
      | x == y = encodeHelper (n + 1) x ys
      | otherwise = encodeHelper' n x : encodeHelper 1 y ys
    encodeHelper' 1 x = Single x
    encodeHelper' n x = Multiple n x

-- 14. Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : x : dupli xs

dupli' :: [a] -> [a]
dupli' list = concat [[x, x] | x <- list]

dupli'' :: [a] -> [a]
dupli'' = concatMap (replicate 2)

-- 15. Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli [] n = []
repli xs n = concatMap (replicate n) xs

repli' :: [a] -> Int -> [a]
repli' xs n = concatMap (take n . repeat) xs

-- 16. Drop every N'th element from a list.
myDrop :: [a] -> Int -> [a]
myDrop xs n
  | length xs < n = xs
  | otherwise = take (n - 1) xs ++ myDrop (drop n xs) n

-- 17. Split a list into two parts; the length of the first part is given.
mySplit :: [a] -> Int -> ([a], [a])
mySplit xs n = (take n xs, drop n xs)

mySplit' :: [a] -> Int -> ([a], [a])
mySplit' xs n = splitAt n xs

-- 18. Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included).
mySlice :: [a] -> Int -> Int -> [a]
mySlice xs i k = take (k - i + 1) (drop (i - 1) xs)

mySlice' :: [a] -> Int -> Int -> [a]
mySlice' xs i k = map (xs !!) [i - 1 .. k - 1]

-- 19. Rotate a list N places to the left.

myRotate :: [a] -> Int -> [a]
myRotate xs n
  -- If n is negative, rotate to the right
  | n >= 0 = drop n xs ++ take n xs
  | otherwise = drop (length xs + n) xs ++ take (length xs + n) xs

-- 20. Remove the K'th element from a list.
myRemoveAt :: Int -> [a] -> (a, [a])
myRemoveAt n xs = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)

myRemoveAt' :: Int -> [a] -> (a, [a])
myRemoveAt' 1 (x : xs) = (x, xs)
myRemoveAt' n (x : xs) = (l, x : r)
  where
    (l, r) = myRemoveAt' (n - 1) xs