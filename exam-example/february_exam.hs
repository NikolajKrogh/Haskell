-- PROBLEM 1

-- 1.1
remove :: [a] -> Int -> [a]
remove [] k = []
remove (x : xs) k
  | k <= 1 = xs
remove (x : xs) k = x : remove xs (k - 1)

-- 1.2
removals :: [a] -> [[a]]
removals [] = []
removals xs = removals' xs (length xs)
  where
    removals' xs 0 = []
    removals' xs k = [remove xs i | i <- [1 .. length xs]]

-- PROBLEM 2

-- 2.1
data Person = Famous String | Ordinary String

data Unit = Single Person | Couple (Person, Person)

data Dynasty = A Unit [Dynasty]

-- dynastyExample = A Couple (Bob, Carol)
-- 2.2

-- PROBLEM 3

-- 3.1

dwindle :: [a] -> [[a]]
dwindle [] = [[]]
dwindle [x] = [[x]]
dwindle (x : xs) = (x:xs) : dwindle xs

-- 3.2
dwindle' :: [a] -> [[a]]
dwindle' xs = [drop i xs | i <- [0 .. length xs]]

-- 3.3
dwindle'' :: [a] -> [[a]]
dwindle'' xs = foldr (\x acc -> (x : head acc) : acc) [[]] xs

-- PROBLEM 4

data Status a = Fresh a | Used a deriving Show
instance Functor Status where
  fmap f (Fresh x) = Fresh (f x)
  fmap f (Used x) = Used (f x)

-- 4.1
instance Applicative Status where
  pure = Fresh
  --(<*>) :: Status (a -> b) -> Status a -> Status b
  (Fresh f) <*> (Fresh x) = Fresh (f x)
  (Fresh f) <*> (Used x) = Used (f x)
  (Used f) <*> (Used x) = Used (f x)
  (Used f) <*> (Fresh x) = Used (f x)

  -- 4.2
instance Monad Status where
  return = pure
  -- (>>=) :: Status a -> (a -> Status b) -> Status b
  Fresh x >>= f = f x
  Used x >>= f = f x

-- 4.3
minimize :: Ord b => Status b -> Status b -> Status b
minimize x y = do
  a <- x
  b <- y
  return(min a b)

-- PROBLEM 5

-- 5.1
--func1:: (Num a) => Bool -> a -> (a, a)
func1 True x = (x+1, x+2)

-- 5.2
--func2:: a1 -> a2 -> (a2, a2) -> (a1, [a2])
func2 x a (b,c) = (x,[a,b,c])

--func3:: Num a => Maybe [a -> a]
func3 = Just [\x -> x+x] -- Correct if you ask the compiler with :t func3 = Just [\x -> x+x]

--func4:: p1 -> p2 -> [a] 
func4 x y = []

-- PROBLEM 6

-- 6.1 
--squares [] = []
--squares (x:xs) = x*x : squares xs 
squaresList n = [n^2] ++ (squaresList (n+1))
squares = squaresList 1 

-- 6.2
squares' = map (\x -> x*x) [1..]

-- 6.3
squares'' = zipWith (*) [1..] [1..]