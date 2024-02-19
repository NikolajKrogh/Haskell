
-- Exercise 1
allbutsecond :: [a] -> [a]
allbutsecond (x : xs : xss) = x : xss

-- Exercise 2
midtover :: [a] -> ([a], [a])
midtover xs = splitAt (length xs `div` 2) xs

-- Exercise 3
bingo ( x , y ) = x mod z
    where
    z = y + 42
