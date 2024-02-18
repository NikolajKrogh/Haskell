-- | Given a target value and a list of coin denominations, `findFewestCoins` attempts to find the fewest number of coins needed to make up the target value.
-- If a valid combination of coins is found, it returns `Just` the list of coins.
-- If no valid combination is found, it returns `Nothing`.
findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
    | target < 0 = Nothing
    | target == 0 = Just []
    | otherwise = findFewestCoins' target coins

-- | Helper function for `findFewestCoins` that recursively finds the fewest number of coins needed to make up the target value.
findFewestCoins' :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins' target coins = case validOptions of
    [] -> Nothing
    options -> Just (reverse (minimumByLength options))
    where
    validOptions = [coin:rest | coin <- coins, coin <= target, Just rest <- [findFewestCoins (target - coin) coins]]

-- | Given a list of lists, `minimumByLength` returns the list with the minimum length.
minimumByLength :: [[Integer]] -> [Integer]
minimumByLength = foldr1 (\x acc -> if length x < length acc then x else acc)