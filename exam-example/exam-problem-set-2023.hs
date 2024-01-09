{-Problem 1.1-}
-- Partion is parametric polymorphic
-- This type of polymorphism allows a function or a data type to be written generically,
-- such that it can handle values identically without depending on their type.

{-Problem 1.2-}
partitionList :: (a -> Bool) -> [a] -> ([a], [a])
partitionList p lst = (yes, no)
  where
    yes = [x | x <- lst, p x]
    no = [x | x <- lst, (not (p x))]

{-Problem 1.3-}
partitionRecursion1 :: (a -> Bool) -> [a] -> ([a], [a])
partitionRecursion1 p [] = ([], [])
partitionRecursion1 p (x : xs)
  | p x = (x : fst (partitionRecursion1 p xs), snd (partitionRecursion1 p xs))
  | otherwise = (fst (partitionRecursion1 p xs), x : snd (partitionRecursion1 p xs))

-- If p x is True, it recursively partitions the tail xs and adds x to the first list (because p x is True).
-- If p x is False, it recursively partitions the tail xs and adds x to the second list (because p x is False).

partitionRecursion2 :: (a -> Bool) -> [a] -> ([a], [a])
partitionRecursion2 p [] = ([], [])
partitionRecursion2 p (x : xs) =
  if p x
    then (x : yes, no)
    else (yes, x : no)
  where
    (yes, no) = partitionRecursion2 p xs

-- ghci> partitionRecursion (=="la") ["tra", "la", "la", "tra", "la"]

{-Problem 2.1-}
data MixedTree a b = Empty | ALeaf a | BLeaf b | ANode a (MixedTree a b) (MixedTree a b) | BNode b (MixedTree a b) (MixedTree a b)

{-Problem 2.2-}
fig1a :: MixedTree String Integer
fig1a = BNode 4 (ANode "dudu" (BLeaf 2) (BLeaf 9)) (ANode "zyzy" (BLeaf 6) (BLeaf 7))

{-
       4
     /    \
"dudu"   "zyzy"
 /   \    /   \
2     9   6    7
-}

fig1b :: MixedTree String Integer
fig1b = ANode "soso" (BNode 3 (BLeaf 10) (ALeaf "bibi")) (ANode "dada" (ALeaf "nene") Empty)

{-
      "soso"
      /      \
     3       "dada"
   /  \       /   \
  10 "bibi" "nene"
-}

{-Problem 2.3-}
islayered :: MixedTree a b -> Bool
islayeredWith :: MixedTree a b -> Label -> Bool

data Label = A | B

islayeredWith Empty _ = True
islayeredWith (ALeaf _) A = True
islayeredWith (BLeaf _) B = True
islayeredWith (ANode _ t1 t2) A = islayeredWith t1 B && islayeredWith t2 B
islayeredWith (BNode _ t1 t2) B = islayeredWith t1 A && islayeredWith t2 A
islayeredWith _ _ = False

islayered t = islayeredWith t A || islayeredWith t B

{-
If the tree is Empty, it's considered layered.
If the tree is an ALeaf and the label is A, or if the tree is a BLeaf and the label is B, it's considered layered.
If the tree is an ANode and the label is A, it checks if both children are layered with label B.
If the tree is a BNode and the label is B, it checks if both children are layered with label A.
In all other cases, the tree is not considered layered.
The underscore _ is used to indicate that the value inside the ALeaf can be anything and its value doesn't matter. This means that islayeredWith (ALeaf x) A will return True for any x
-}

{-Problem 3.1-}
-- This is both because it contains a type variable that are not constrained by a class constraint and a variable that is
-- problem31 :: (Eq a, Num a) => a -> a -> [b] -> (b, b)
problem31 x y zs = if x == y + 2 then (head zs, head zs) else (head zs, head zs)

{-Problem 3.2-}
-- This is both because it contains a type variable that are not constrained by a class constraint and a variable that is
-- problem32 :: (Show a, Fractional a) => p -> a -> [Char]
problem32 x y = show (y / 2.0)

{-Problem 3.3-}
-- This is parametric polymorphic only
-- problem33 :: [a] -> [a] -> [a]
problem33 (x : xs) ys = x : ys

{-Problem 3.4-}
-- This is parametric polymorphic only
-- problem34 :: (t1 -> t2) -> ((b,b) -> t1) -> b -> t2
problem34 f g y = f (g (y, y))

{-Problem 4.1-}
-- lists is only ad hoc-polymorphism because it only contains type variables that are constrained by a class constraint (=>)
-- This type of polymorphism allows a function to have different implementations depending on the types of its arguments.

{-Problem 4.2-}
listsRecursion :: (Eq a, Num a) => a -> [[a]]
listsRecursion n = (copies n n) : (listsRecursion (n + 1))

copies n 0 = []
copies n k = n : (copies n (k - 1))

-- OR
-- This is parametric polymorphic
listsRecursion' n = (replicate n n) : (listsRecursion (n + 1))

{-Problem 4.3-}
listsHigherOrder n = map (\x -> replicate x x) [n ..]

-- OR
listsHigherOrder' n = map (\x -> copies x x) [n ..]

{-Problem 5.1-}
newtype Funpair a = Fun (Bool -> a, String -> a)

-- myPair:: Funpair Int
myPair = Fun (f, g)
  where
    f True = 42
    f False = 69
    g "Hello World" = 420
    g _ = 1984

myOtherPair = Fun (\_ -> 42, \_ -> 69)

{-Problem 5.2-}
instance Functor Funpair where
  fmap f (Fun (g, h)) = Fun (f . g, f . h)

{-Problem 5.3-}
-- functor law
{-
fmap id = id
fmap id (Fun(f1,f2)) =
Fun ((id . f1), (id . f2)) =
Fun (f1,f1)
and
id(Fun(f1,f2)) = Fun(f1,f2)
-}

{-Problem 6.1-}
data W x = Bingo x deriving (Show)

instance Functor W where
  fmap f (Bingo x) = Bingo (f x)

instance Monad W where
  -- return x = Bingo x
  Bingo x >>= f = f x

instance Applicative W where
  pure = Bingo
  Bingo f <*> Bingo x = Bingo (f x)

{-Problem 6.2-}
wrapadd :: Int -> W Int -> W Int
wrapadd x y = do
  b <- y
  return (x + b)

h :: W Int -> W Int -> W Int
h x y = do
  a <- x
  b <- y
  return (a * b)