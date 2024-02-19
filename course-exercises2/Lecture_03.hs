-- Exercise 1
twice :: (a -> a) -> a -> a
twice f x = f (f (x))

twicetwo :: (a -> a, a) -> a
twicetwo (f, x) = f (f (x))

-- Exercise 3
dingo :: (a, a) -> [a]
dingo (x, y) = [x, y]

-- b)
bingo :: a -> a
bingo x = x

-- c)
thesame :: Eq a => [(a, a)] -> [(a, a)]
thesame xs = filter (uncurry (==)) xs

-- e)
double :: Num b => [b] -> [b]
double xs = map (* 2) xs

-- f)
findType :: (Ord a1, Eq a2) => a2 -> a2 -> (a1, a1) -> a1
findType x y (a, b)
  | x == y = a
  | otherwise = b

-- g)
madras :: (t -> t -> t, t, t) -> t
madras (f,x,y) = f (f x x) y

madrasCurried f x y = f (f x x) y