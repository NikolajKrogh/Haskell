--allbutsecond (x:y:xs) = x : xs

-- first exercise
allbutsecond (x:xs) = x : tail xs

-- second exercise
midtover xs = splitAt(length xs `div` 2) xs

-- third exercise
bingo ( x , y ) = x `mod` z
    where
    z = y + 42

{-
There is a function called reverse in the Haskell prelude that allows us to reverse any list. Use reverse
to give a definition of a function final that returns the last element of a given list.
-}

-- final xs = head (reverse (xs))
final = last

{-
How can we change qsort from simple.hs such that it sorts a list in descending order? There are more
ways of approaching this. Do the calls
-}

qsort :: (Ord a) => [a] -> [a]

qsort [] = []
qsort (x:xs) = big ++ [x] ++ small
                 where small = qsort [a | a <- xs, a <= x]
                       big   = qsort [a | a <- xs, a > x]