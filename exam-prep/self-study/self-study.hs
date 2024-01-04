{-1-}
triples:: Num a => [(a,a,a)] -> ([a],[a],[a])
triples [] = ([],[],[])
triples [(a,b,c)] = ([a],[b],[c])
triples ((x,y,z):xs) = (x:xs', y:ys', z:zs')
    where (xs', ys', zs') = triples xs

{-3-}
--alone is ad-hoc polymorphic 
alone:: Ord a => [a] -> [a]
alone [] = []
alone [x] = [x]
alone xs = [x | x <- xs, length (filter (==x) xs) == 1]

{-4-}
mapusingfoldr :: (a -> b) -> [a] -> [b]
mapusingfoldr f xs = foldr(\x xs -> f x:xs) [] xs

{-5-}
func1::(Ord a, Num a)=>a -> a -> [[Bool]] -> Bool
func1 z y xs 
    | z > y = True
    | otherwise = False

func2::Num a =>(t -> a, t)-> a -> a
func2 (f, x) y = f x + y

func3::Fractional t1 =>(t2 -> t1)-> (t2 -> t1)-> (t1 -> t3)-> t2 -> t3
func3 f g h x = h (f x / g x)

{-6-}
-- This function is parametric polymorphic which allows a function or a data type to be written generically, so that it can handle values identically without depending on their type.
replaceAtPosition :: [a] -> a -> Int -> [a]
replaceAtPosition xs value at = take at xs ++ [value] ++ drop (at + 1) xs

{-7-}
lgdsort::Ord a => [a] -> [a]
lgdsort [] = []
lgdsort (x:xs) = smaller ++ [x] ++ larger
    where smaller = lgdsort [a | a <- xs, a <= x]
          larger = lgdsort [a | a <- xs, a > x]

{-8-}
whilefunc::(a->Bool) -> [a] -> [a]
whilefunc pred = takeWhile pred 

{-9-}
noDuplicatesAdjacentRecursion::Eq a => [a] -> [a]
noDuplicatesAdjacentRecursion [] = []
noDuplicatesAdjacentRecursion (x:y:xs)
    | x==y = noDuplicatesAdjacentRecursion(y:xs)
    | otherwise = x:noDuplicatesAdjacentRecursion (y:xs)
noDuplicatesAdjacentRecursion xs = xs

noDuplicatesAdjacentFoldr :: Eq a => [a] -> [a]
noDuplicatesAdjacentFoldr [] = []
noDuplicatesAdjacentFoldr xs = foldr (\x acc -> if null acc || x /= head acc then x : acc else acc) [] xs

noDuplicatesAdjacentFoldl::Eq a => [a] -> [a]
noDuplicatesAdjacentFoldl [] = []
noDuplicatesAdjacentFoldl xs = foldl (\acc x -> if null acc || x /= last acc then acc ++ [x] else acc) [] xs
