-- PROBLEM 1

-- 1.1
-- rotate is parametric polymorphic
-- This type of polymorphism allows a function or a data type to be written generically,
-- such that it can handle values identically without depending on their type.
rotate :: [a] -> [a]
rotate [] = []
rotate (x : xs) = xs ++ [x]

-- 1.2
allrotates :: [a] -> [[a]]
allrotates [] = []
allrotates [x] = [[x]]
allrotates xs = take n (infiniteNumberRotations xs)
  where
    n = length xs
    infiniteNumberRotations xs = xs : infiniteNumberRotations (rotate xs)

-- 1.3
allrotates' :: [a] -> [[a]]
allrotates' [] = []
allrotates' xs = reverse (foldr (\_ acc -> rotate (head acc) : acc) [xs] [1 .. length xs - 1])

allrotates'' xs = reverse (foldr (\x (h : t) -> (rotate h) : (h : t)) [xs] [1 .. length xs - 1])

-- PROBLEM 2

-- 2.1
data Tree a = Leaf a | InternalNode (Tree a) (Tree a) | Node (Tree a) a (Tree a) deriving (Show)

treeInt = InternalNode (InternalNode (Leaf 17) (Leaf 484000)) (Leaf 1964)

treeString = Node (Leaf "plip") "bingo" (Node (Leaf "uhu") "plop" (Leaf "fedtmule"))

-- 2.2
isfull :: Tree a -> Bool
isfull (Leaf _) = True
isfull (InternalNode _ _) = False
isfull (Node left _ right) = isfull left && isfull right

-- 2.3
preorder :: Tree a -> Maybe [a]
preorder (Leaf x) = Just [x]
preorder (InternalNode _ _) = Nothing
preorder (Node left x right) =
  do
    l1 <- preorder left
    l2 <- preorder right
    return (x : (l1 ++ l2))

-- PROBLEM 3
removeListComprehension :: String -> String -> String
removeListComprehension [] ys = ys
removeListComprehension xs ys = [y | y <- ys, not (y `elem` xs)]

remove :: String -> String -> String
remove xs ys = filter (`notElem` xs) ys

-- PROBLEM 4
newtype WrapString a = WS (a, String) deriving (Show)

instance Functor WrapString where
  -- fmap :: (a -> b) -> f a -> f b
  -- fmap :: (a -> b) -> WrapString a -> WrapString b
  fmap f (WS (x, s)) = WS (f x, s)

-- Fuctor = Tillad map over datatyper

instance Applicative WrapString where
  -- pure :: a -> WrapString a
  pure x = WS (x, "horse")

  -- (<*>) :: f (a -> b) -> f a -> f b
  -- (<*>) :: WrapString (a -> b) -> WrapString a -> WrapString b
  (<*>) (WS (g, str1)) (WS (x, str2)) = WS (g x, str1)

-- (<*>) (WS(g,_)) wx = fmap g wx

instance Monad WrapString where
  -- return :: a -> m a
  -- return :: a -> WrapString a
  return = pure

  -- or return x = WS(x, "")
  -- (>>=) :: WrapString a -> (a -> WrapString b) -> WrapString b
  WS (x, _) >>= f = f x

pairup :: WrapString a -> WrapString b -> WrapString (a, b)
pairup wx wy = do
  x <- wx
  y <- wy
  return (x, y)

-- data Tree1 a = Leaf a | Node (Tree1 a) (Tree1 a) deriving Show

-- instance Functor Tree1 where
-- -- fmap :: (a -> b) -> Tree a -> Tree b
--     fmap f (Leaf x) = Leaf (f x)
--     fmap f (Node l r) = Node(fmap f l) (fmap f r)

-- data Tree2 a = Node2 a [Tree2 a]
-- instance Functor Tree2 where
-- -- fmap :: (a -> b) -> Tree a -> Tree b
--     fmap f (Node2 x []) = (Node2 (f x)) []
--     fmap f (Node2 x xs) = (Node2 (f x)) (map (fmap f) xs)

-- PROBLEM 5

-- 5.1
-- func1:: (Ord a, Num a) => a -> a -> a -> (a, a)
func1 x y z = if x + 1 > y then (x, y) else (x, z)

-- 5.2
-- func2:: [(Integer, p -> Char)]
func2 = [(1, const 'a')]

func2' = [(4, \x -> 'a')]

-- 5.3
-- func3:: (t1 -> Bool -> t2) -> t1 -> t2
func3 f x = f x True

-- 5.4
-- d:: (Num a, Enum a) => [a]
-- [1..3]

-- PROBLEM 6

-- 6.1
naturals :: [Integer]
naturals = generate 0
    where 
        generate n = n : generate (n+1) 

-- 6.2
--map :: (a -> b) -> [a] -> [b]
facs = map(\n -> product[1..n]) [1..]

facs2 = map factorial [0..]
    where
        factorial 0 = 1
        factorial n = n * factorial (n-1)

facs' = 1 : zipWith (*) [1..] facs'
