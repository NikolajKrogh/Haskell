{- 1.
Define a function product that multiplies the elements of a given list of numbers. If the list is
empty, the product should be 0. You should to this by modifying a definition from Section 1.5.
-}

mult::Num a => [a] -> a
mult [] = 1
mult (x:xs) = x * mult xs

{- 2.
Define a function second that will, when given a list, return the second element of the list if it
exists. As examples of what this function should do, we expect that
second [1,4,5,6]
will give us 4, and that
second ["some", "bizarre", "mango"]
will give us ”bizarre”. Show that your definition of second works for these examples of arguments.
Then find two more examples of arguments and see what happens. Is your function a total function?-}

second::[a] -> a
second (x:xs) = head xs

second' :: [a] -> Maybe a
second' [] = Nothing
second' (_:[]) = Nothing
second' (_:x:_) = Just x

{-1-}
allbutsecond (x:xs) = x : tail xs

{-2-}
midtover xs = splitAt(length xs `div` 2) xs

{-3
What is wrong with this code?
bingo (x,y) = x mod z
where
z = y + 42
-}
-- The indentation is not correct
-- mod should have backward ticks `mod`
bingo (x,y) = x `mod` z
    where
    z = y + 42

{-a-}

final xs = head(reverse xs) 