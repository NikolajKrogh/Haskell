-- Name: Nikolaj Kofod Krogh
-- AAU mail address: nkrogh20@student.aau.dk
-- Study number: 20205320

-- PROBLEM 1

-- 1.1
-- descending is only ad hoc-polymorphic because it contains a type variable (a) that is constrained by a class constraint (=>)
-- Bool is not polymorphic
descending :: Ord a => [a] -> Bool
descending [] = True
descending [x] = True
descending (x : y : xs)
  | x > y && descending (y : xs) = True
  | x == y && descending (y : xs) = True
  | otherwise =
      False

-- 1.2
-- segments is only ad hoc-polymorphic because it contains a type variable (a) that is constrained by a class constraint (=>)
segments :: Eq a => [a] -> [[a]]
segments [] = []
segments (x : xs) = (x : takeWhile (== x) xs) : segments (dropWhile (== x) xs)

-- PROBLEM 2

-- 2.1

data Encyclopedia key value = Branch1 key value (Encyclopedia key value) | Branch2 key value (Encyclopedia key value) (Encyclopedia key value) | Branch3 key value (Encyclopedia key value) (Encyclopedia key value) (Encyclopedia key value) | Leaf key value deriving (Show)

t1 = Branch3 "mango" True (Branch2 "dingo" False (Leaf "plip" True) (Leaf "ninka" True)) (Leaf "plop" True) (Branch1 "plys" False (Leaf "boing" True))

t2 = Branch3 "plonk" 1 (Branch1 "zap" 3 (Leaf "ninka" 8)) (Branch1 ("uhu") 4 (Leaf "gif" 9)) (Leaf "bingo" 5)

-- 2.2
-- containskey (Encyclopedia a _) key = (containskey any key (a))
-- containskey encyclopedia key = any key encyclopedia

-- containskey (Leaf key value) checkkey = key == checkkey
-- containskey (Branch1 key value (Leaf leafkey leafvalue)) checkkey = key || leafkey == checkkey
-- 2.3

-- PROBLEM 3

-- 3.1
interleave :: [a] -> [a] -> [a]
interleave [] [] = []
interleave xs [] = xs
interleave [] ys = ys
interleave (x : xs) (y : ys) = x : y : interleave xs ys

-- 3.2
-- map :: (a -> b) -> [a] -> [b]
interleave' xs (y : ys) = concat (map (\_ -> y : xs) xs)

interleave''' xs (y : ys) = concat (map (\_ -> y : xs) xs)

-- zip :: [a] -> [b] -> [(a, b)]
interleave'' xs ys = (zip xs ys)

-- Min tanke var at bruge pairstolist til at trække dem ud af pairs, så det bare er en list: pairstolist(zip xs ys)
pairstolist xs = [x y | (x, y) <- xs]

-- PROBLEM 4

-- 4.1
data PExp = N Int | X String | Add PExp PExp deriving (Show)

-- 4.2
data State = Var Int | Empty

-- 4.3

{-
eval :: PExp e -> State a -> Maybe e
eval (Add 4 5) (Var 9)  = do
    x <- state
    return state
-}
-- PROBLEM 5

-- 5.1
-- Here there are both types of polymorphism both ad-hoc and parametric.
-- ad-hoc because we overload (=>) b
-- parametric polymorphism because (a) is not affected by the overload
-- func1:: Num b => a −> b −> ([a], b)
func1 x y = ([x + 1], y)

-- 5.2
-- This is only parametric polymorphic which means we can handle the values identically without depending on their type.
-- func2:: (t1 −> t2 −> t1 −> t3) −> t1 −> t2 −> t3
func2 f x y = f x y x

-- 5.3
-- This is neither ad-hoc or parametric in this function since we always know it takes a function and returns a Bool
-- func3:: Maybe [p −> Bool]
func3 = Just [\f -> True]

-- 5.4
-- This is only parametric polymorphic which means we can handle the values identically without depending on their type.
-- func4:: (t1 -> a) -> t1 -> (t2 -> a) -> t2 -> Int
func4 f x g y = 42
  where
    helper1 = f x
    helper2 = g y

-- PROBLEM 6

-- 6.1
{-
harmonicSequence = harmonicSequenceFrom 1.0
  where
    harmonicSequenceFrom n = n `div` n : (harmonicSequenceFrom (n))
-}
{-
harmonicList n = [n`div`n] ++ (harmonicList (n))
harmonicSequence = harmonicList 1
-}
-- 6.2

-- 6.3
