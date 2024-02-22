-- Name: Nikolaj Kofod Krogh
-- AAU mail address: nkrogh20@student.aau.dk
-- Study number: 20205320

-- PROBLEM 1

-- 1.1

-- myRotate is parametric polymorphic
-- This type of polymorphism allows a function or a data type to be written generically,
-- such that it can handle values identically without depending on their type.
myRotate :: [a] -> Int -> [a]
myRotate xs n
  -- If n is negative, rotate to the right
  | n >= 0 = drop n xs ++ take n xs
  | otherwise = drop (length xs + n) xs ++ take (length xs + n) xs

myRotateTestInt = myRotate [1 .. 9] 1

myRotateTestString = myRotate "Hello" 1

-- 1.2
-- This does not work because it runs in an endless loop and only rotate the first elements in the list, but
-- it is still parametric polymorphic as it contains no class constraint i.e. => this means that it can handle values identically without depending on their type.
allrotates :: [a] -> [[a]]
allrotates [] = []
allrotates [x] = [[x]]
allrotates (x : y : xs) = l1 : l2
  where
    l1 = x : y : xs
    l2 = allrotates (y : x : xs)

-- 1.3

-- PROBLEM 2

-- 2.1
{-data Tree a = Leaf a | PartialBranch (Tree a) (Tree a) | Branch a (Tree a) (Tree a) deriving (Show)

t1 = PartialBranch (PartialBranch (Leaf 17) (Leaf 484000)) (Leaf 1964)

t2 = Branch "bingo" (Leaf "plip") (Branch "plop" (Leaf "uhu") (Leaf "fedtmule"))

-- 2.2
isfullWith :: Tree a -> Label -> Bool
isfull :: Tree a -> Bool

data Label = Full | Partial

isfullWith (Branch _ t1 t2) Full = True
isfullWith (PartialBranch _ t1) Partial = False
isfullWith _ _ = False

isfull t = isfullWith t Full
-}
data Tree a = Label a (Tree a) (Tree a) | Unlabel (Tree a) (Tree a) | Leaf a

t1 = Label "goat" (Unlabel (Leaf "pig") (Leaf "cow")) (Leaf "sheep") -- Should return False

t2 = Label "goat" (Label "horse" (Leaf "pig") (Leaf "cow")) (Leaf "sheep") -- Should return true

isfull (Leaf _) = True
isfull (Unlabel _ _) = False
isfull (Label _ l r) = isfull l && isfull r
-- 2.3

-- PROBLEM 3

-- 3.1

-- remove :: String -> String -> String
-- remove xs (y:ys) = [x | x<-xs, if x == y then xs else y:xs ]

removefilter :: String -> String -> String
removefilter [] ys = ys
removefilter xs ys = filter (`notElem` xs) ys

removefoldr :: String -> String -> String
removefoldr xs ys = foldr (\y acc -> if y `elem` xs then acc else y : acc) [] ys

-- 3.2

-- PROBLEM 4

-- 4.1
newtype WrapString a = WS (a, String) deriving (Show)

instance Functor WrapString where
  fmap f (WS (x, s)) = WS (f x, s)

{-
instance Applicative WrapString where
 pure a = WS (a, "")
 <*> :: f (a -> b) -> f a -> f b
 WS f <*> WS g = WS (f x, g s)
-}
-- 4.2

-- 4.3

-- PROBLEM 5

-- 5.1
-- This is only ad hoc-polymorphism because it only contains type variables that are constrained by a class constraint (=>)
-- This type of polymorphism allows a function to have different implementations depending on the types of its arguments.
-- func1 :: (Ord a, Num a) => a -> a -> a -> (a, a)
func1 x y z = if x >= y then (x, z) else (z + y, y)

-- 5.2
-- This is only parametric polymorphism and this type of polymorphism allows a function or a data type to be written generically, such that it can handle values identically without depending on their type.
-- func2::[(Integer, p -> Char)]
func2 = [(42, 'c')]

-- 5.3
-- This is only parametric polymorphism and this type of polymorphism allows a function or a data type to be written generically, such that it can handle values identically without depending on their type.
-- func3 ::(t1 −> Bool −> t2) −> t1 −> t2
func3 f x y = f (x True y)

-- 5.4
-- This is only ad hoc-polymorphism because it only contains type variables that are constrained by a class constraint (=>)
-- This type of polymorphism allows a function to have different implementations depending on the types of its arguments.

-- func4 :: (Num a, Enum a) => [a]
-- ghci> :t [1..3]

-- PROBLEM 6

-- 6.1
-- naturals :: (Num a) => a -> [a]
natural = 0

naturals :: [Integer]
naturals = generate natural
  where
    generate natural = natural : generate (1 + natural)

naturals2 n = n : naturals2 (n+1)
-- 6.2

-- map :: (a -> b) -> [a] -> [b]
facs :: (Num a, Enum a) => a -> [a]
facs n = map (\a -> 1 * product [1 .. n]) [1 .. n]

factorial :: (Num a, Enum a) => a -> a
factorial n = foldr (*) 1 [1 .. n]

-- 6.3