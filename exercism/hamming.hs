-- filter usually expect a predicate. We can use id to keep only the True values (we do not care about what the original values are)
-- zipWith generates a boolean list where it is True if the elements are different and otherwise False
keepOnlyDifferent :: Eq b => [b] -> [b] -> [Bool]
keepOnlyDifferent xs ys = filter id (zipWith (/=) xs ys)

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = Just (length (keepOnlyDifferent xs ys))




distanceRecursive :: String -> String -> Maybe Int
distanceRecursive [] [] = Just 0
-- Recursive case: If the first characters of the strings are equal, 
-- it recursively calculates the Hamming distance for the rest of the strings and adds 0 to the result.
-- If they are not equal, it adds 1 to the result.
distanceRecursive (x:xs) (y:ys)
  | x == y    = fmap (+0) (distanceRecursive xs ys)
  | otherwise = fmap (+1) (distanceRecursive xs ys)
-- If the strings are of different lengths, it returns 'Nothing'.
distanceRecursive _ _ = Nothing