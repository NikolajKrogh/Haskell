removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit
  | null factors = 0
  | minimum factors >= limit = 0
  | otherwise = let factors' = filter (/= 0) factors
                in sum [x | x <- [1 .. limit - 1], any (\f -> x `mod` f == 0) factors']



-- | Calculates the sum of multiples of given factors up to a specified limit.
--
-- The function takes a list of factors and a limit as input. It generates a list of numbers from 1 to (limit - 1),
-- and filters out the numbers that are not divisible by any of the factors. Finally, it returns the sum of the remaining numbers.
--
-- Examples:
-- >>> sumOfMultiples2 [3, 5] 10
-- 23

-- The function assumes that the factors are positive integers and the limit is a positive integer greater than 1.
sumOfMultiples2 :: [Integer] -> Integer -> Integer
sumOfMultiples2 factors limit = sum [x | x <- [1 .. limit - 1], any ((== 0) . (x `mod`)) (filter (/= 0) factors)]
