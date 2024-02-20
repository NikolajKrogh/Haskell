-- Exercise 1
-- one :: (Ord a, Num a) => a -> a -> [[Bool]] -> Bool
one x y [[True]] = if x + y > 5 then True else False

-- two :: (Num a) => (t -> a, t) -> a -> a
two (f, x) y = f x + y

-- three :: (Fractional t1) => (t2 -> t1) -> (t2 -> t1) -> (t1 -> t3) -> t2 -> t3
three f g h x = h (f x / g x)

-- Exercise 2 + 3
triples :: (Num a) => [(a, a, a)] -> ([a], [a], [a])
triples [] = ([], [], [])
triples [(a, b, c)] = ([a], [b], [c])
triples ((x, y, z) : xs) = (x : xs', y : ys', z : zs')
  where
    (xs', ys', zs') = triples xs

-- a)
frequencies:: String -> [(Char, Int)] 
frequencies [] = []
frequencies (x : xs) = (x, countElement) : frequencies remainingElements

    where 
        countElement = 1 + length (filter (== x) xs)
        remainingElements = [remove | remove <- xs, remove /= x]

