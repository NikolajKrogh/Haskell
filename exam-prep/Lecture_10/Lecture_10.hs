{-Prep 1-}
data Onion a = Core a | Layer (Onion a) deriving (Show)

instance Functor Onion where
  fmap f (Core a) = Core (f a)
  fmap f (Layer onion) = Layer (fmap f onion)

myOnion = Layer (Layer (Layer (Core 5)))

-- Use fmap to add 1 to the value inside the Onion
myOnionPlusOne = fmap (+ 1) myOnion

{-1-}
data UTree a = Node a [UTree a] deriving (Show)

instance Functor UTree where
  fmap f (Node tree xs) = Node (f tree) (fmap (fmap f) xs)

-- ghci> fmap (+1) testTree
testTree = Node 1 [Node 2 [Node 3 []]]

testTree2 = Node 1 [Node 3 [Node 5 [Node 7 []]]]

{-2-}
{-
instance Functor ((->)r) where
    fmap f g = (\x -> f(g x))
-}

{-3-}
{-
instance Applicative [] where
  --(<*>)::[a->b] -> [a] -> [b]
  pure x = [x]
  (g:gs) <*> xs = case gs of
    [] -> []
    (g:gs') -> fmap g xs ++ (gs' <*> xs)
-}

{-4-}
prodthree :: (Num a) => [a] -> [a] -> [a] -> [a]
prodthree [] [] [] = []
-- (\x y z -> x*y*z) is a function that takes three numbers and returns their product.
-- <$> is an infix version of fmap. It applies the function to the first list xs.
-- <*> is a method of the Applicative type class. It takes a value in a context (in this case, a list of functions) and a value in the same kind of context (in this case, the list ys)
prodthree xs ys zs = (\x y z -> x * y * z) <$> xs <*> ys <*> zs

{-a-}
data Exp a = Var a | Val Integer | Add (Exp a) (Exp a) | Mult (Exp a) (Exp a) deriving (Show)
instance Functor Exp where
  --fmap f (Var a) applies the function f to a and wraps the result in the Var constructor.
  fmap f (Var a) = Var (f a)
  --fmap _ (Val i) ignores the function and just returns the Val constructor with the same integer i.
  fmap _ (Val i) = Val i
  --fmap f (Add e1 e2) applies fmap f recursively to e1 and e2 and wraps the results in the Add constructor.
  fmap f (Add exp1 exp2) = Add (fmap f exp1) (fmap f exp2)
  fmap f (Mult exp1 exp2) = Mult (fmap f exp1) (fmap f exp2)

{-b-}
instance Applicative Exp where
  pure = Var
  Var function <*> value = fmap function value

{-c-}
{-type Name = String
type Env = [(Name, Int)]

fetch:: Name -> Env -> Int
fetch x env = case lookup x env of
  Nothing -> error "invalid name"
  Just v -> v

eval:: Exp -> Env -> Int
eval (Var x) env = fetch x env
eval (Val i) _ = i
eval (Add e1 e2) env = eval e1 env + eval e2 env
eval (Mult e1 e2) env = eval e1 env * eval e2 env
-}