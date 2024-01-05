{-Prep 1-}
data Unary = I Unary | Z deriving (Show)

unary2int :: Unary -> Integer
unary2int Z = 0
unary2int (I n) = 1 + unary2int n

-- unary2int (I (I (I (I Z))))

{-Prep 2-}
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

least :: (Ord a) => Tree a -> a
least (Leaf a) = a
least (Node l x r) = minimum [least l, x, least r]

-- let t = Node (Leaf 1) 2 (Leaf 3)

{-1-}
data Aexp = Num Integer | Var String | Add Aexp Aexp | Mult Aexp Aexp

{-2-}
type Association k v = [(k, v)]

find :: (Eq k) => k -> Association k v -> v
-- find that takes two arguments: an association list ass and a key k. The association list ass is a list of key-value pairs (tuples).
-- The function uses a list comprehension to generate a list of all values v for which the key k' is equal to the input key k.
find k ass = head [v | (k', v) <- ass, k' == k]

eval :: Aexp -> Association String Integer -> Integer
eval (Num n) _ = n
eval (Var x) ass = v
  where
    v = find x ass
eval (Add e1 e2) ass = v1 + v2
  where
    v1 = eval e1 ass
    v2 = eval e2 ass
eval (Mult e1 e2) ass = v1 * v2
  where
    v1 = eval e1 ass
    v2 = eval e2 ass

assignment = [("x",3),("y",4)]
--let expr = Add (Mult (Num 2) (Var "x")) (Var "y")
--let result = eval expr assignment

{-3-}