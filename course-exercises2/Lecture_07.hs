-- Exercise 1
data Aexp = Num Integer | Variable String | Add Aexp Aexp | Mult Aexp Aexp deriving Show

-- Exercise 2
type Association k v = [(k,v)] 


find :: (Eq k) => k -> Association k v -> v
find k ass = head [v | (k',v) <- ass, k == k']

eval :: Aexp -> Association String Integer -> Integer
eval (Num n) _ = n
eval (Variable x) ass = v
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

-- assignment = [("x",3),("y",4)]
-- main :: IO ()
-- main = do
--     let result = eval (Add (Mult (Num 2) (Variable "x")) (Variable "y")) assignment
--     print result


-- Exercise 4
data Tree a = Leaf a | Empty | Node ( Tree a ) a ( Tree a ) deriving Show

insert :: Ord a => Tree a -> a -> Tree a
insert Empty x = Leaf x
insert (Leaf y) x
    | x < y = Node (Leaf x) y Empty
    | x > y = Node Empty y (Leaf x) 
    | otherwise = Leaf x
insert (Node left y right) x 
    | x < y = Node (insert left x) y right
    | x > y = Node left y (insert right x)
    | otherwise = Node left x right

-- main :: IO ()
-- main = do
--     let tree = Leaf 10
--     let tree' = insert tree 5
--     let tree'' = insert tree' 15
--     print tree''


-- a)
numLeaves :: Tree a -> Int
numLeaves Empty = 0
numLeaves (Leaf _) = 1
numLeaves (Node left _ right) = numLeaves left + numLeaves right

balanced :: Tree a -> Bool
balanced Empty = True
balanced (Leaf _) = True
balanced (Node left _ right) = abs (numLeaves left - numLeaves right) <= 1 && balanced left && balanced right


-- b)
data Prop = Const Bool| Not Prop | And Prop Prop | Or Prop Prop | Imply Prop Prop | Equiv Prop Prop deriving Show
type Substitution = [(Char, Bool)]

eval2 :: Substitution -> Prop -> Bool
eval2 _ (Const b) = b
eval2 s (Not p) = not (eval2 s p)
eval2 s (And p q) = eval2 s p && eval2 s q
eval2 s (Or p q) = eval2 s p || eval2 s q
eval2 s (Imply p q) = not (eval2 s p) || eval2 s q
eval2 s (Equiv p q) = eval2 s p == eval2 s q

equiv :: Prop -> Prop -> Substitution -> Bool
equiv p q s = eval2 s p == eval2 s q

main :: IO ()
main = do
    let s = [('A', True), ('B', False)]
    let p = And (Const True) (Not (Const False))
    let q = Const True
    print $ equiv p q s

-- e) build a search tree from a given list.
build :: Ord a => [a] -> Tree a
build = foldl insert Empty

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Leaf x) = [x]
inOrder (Node left x right) = inOrder left ++ [x] ++ inOrder right

sort :: Ord a => [a] -> [a]
sort = inOrder . build