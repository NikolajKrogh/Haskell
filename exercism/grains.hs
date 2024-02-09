square :: Integer -> Maybe Integer
square n = if n > 0 && n<65 then Just (2^(n-1)) else Nothing

total :: Integer
total = let a = 1  -- first term
            r = 2  -- common ratio
            n = 64  -- number of terms
        in a * (r^n - 1) `div` (r - 1)




square2 :: Integer -> Maybe Integer
square2 n
    | n < 1 || n > 64 = Nothing
    | otherwise       = Just (2 ^ (n-1))

total2 :: Integer
total2 = 2 ^ 64 - 1