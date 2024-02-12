discard :: (a -> Bool) -> [a] -> [a]
discard p xs = [x | x <- xs, not (p x)]

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = [x | x <- xs, p x]



discard2 :: (a -> Bool) -> [a] -> [a]
discard2 p = filter (not . p)

keep2 :: (a -> Bool) -> [a] -> [a]
keep2 = filter