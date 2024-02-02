-- Recursive solution to the collatz conjecture problem
collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | n == 1 = Just 0
  | isEven = fmap succ (collatz (n `div` 2))
  | otherwise = fmap succ (collatz (3 * n + 1))
  where
    isEven = n `mod` 2 == 0