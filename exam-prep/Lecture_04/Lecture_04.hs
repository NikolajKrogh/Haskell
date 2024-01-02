{-Prep 1-}
onlytwo:: [a] -> Bool
onlytwo [] = False
onlytwo [x] = False
onlytwo [_,_] = True
onlytwo _ = False

{-Prep 2-}
alldots::Num a => [(a,a)] -> [(a,a)] -> [a]
alldots xs ys  = [a*c + b*d | (a,b) <- xs, (c,d) <- ys]

{-1-}
sevens:: Int -> [Int]
sevens x = [a | a <- [1..x - 1], a `mod` 7==0 ]

{-2-}
pyt:: Int -> [(Int,Int,Int)]
pyt x = [(a, b, c) | c <- [1..x], b <- [1..c], a <- [1..b],  a^2+b^2==c^2, a <= b, b < c]

{-4-}
plonk:: Num a => a -> a -> a -> a
plonk x y z = x+y+z

plonk'::Num a => a -> (a -> a -> a)
plonk' x y z = x+y+z

{-5-}
somefunction:: (Ord a1, Eq a2) =>a2 -> a2 -> (a1, a1) -> a1
somefunction x y (z1, z2) 
    | x == y = z1
    | otherwise = z2


{-a-}
flop::[(a,b)] -> [(b,a)]
flop [] = []
flop xs = [(snd x, fst x) | x <- reverse xs]

{-b-}
dupli::[a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

{-c-}
isperfect:: Int -> [Int]
isperfect a =  [x | x <- [1..a-1], a `mod` x==0]

{-d-}
bigheadnumber:: [Int] -> Int
bigheadnumber (x:xs) = length (filter(> x) xs)
bigheadlist:: [Int] -> [Int]
bigheadlist (x:xs) = [y | y <- xs, y > x]

{-e-}
sums m n = [x+y | x <- [1..m], y <- [1..n]]
sums' m n = concat[[x+y | y <- [1..n]] | x <- [1..m]]