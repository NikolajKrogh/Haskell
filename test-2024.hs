-- Opgave 1

-- 1.1

remove [] k = []
remove (x : xs) k | k <= 1 = xs
remove (x : xs) k = x : (remove xs (k - 1))

-- 1.2

removals xs = [remove xs i | i <- [1 .. length xs]]

-- Opgave 2

-- 2.1

data Person = Famous String | Normal String

data Unit = Single Person | Couple (Person, Person)

data Dynasty = A Unit [Dynasty]

-- 2.2

isfamous (Single (Famous _)) = True
isfamous (Couple (Famous _, Famous _)) = True
isfamous _ = False

famoushead (A u d) = isfamous u

istrendy (A u dyn) = isfamous u && (all famoushead dyn) && (all istrendy dyn)

-- Opgave 3

-- 3.1

dwindle [] = [[]]
dwindle [x] = [[x]]
dwindle (x : xs) = (x : xs) : (dwindle xs)

-- 3.2

dwindle' xs = [(drop n xs) | n <- [1 .. (length xs)]]

-- 3.3

dwindle'' xs = foldr (\x (h : t) -> (tail h) : (h : t)) xs [xs]

-- Opgave 4

-- 4.1

data Status a = Fresh a | Used a deriving (Show)

instance Functor Status where
  fmap f (Fresh x) = Fresh (f x)
  fmap f (Used x) = Used (f x)

instance Applicative Status where
  pure x = Fresh x
  (Fresh f) <*> (Used x) = (Used (f x))
  (Used f) <*> (Fresh x) = (Used (f x))
  (Fresh f) <*> (Fresh x) = (Fresh (f x))
  (Used f) <*> (Used x) = (Used (f x))

-- 4.2

instance Monad Status where
  return = pure

  -- >>= ::  (Status a) -> (a -> Status b) -> (Status b)
  (Fresh x) >>= f = f x
  (Used x) >>= f = f x

-- 4.3

minimize :: Ord b => Status b -> Status b -> Status b
minimize x y = do
  xx <- x
  yy <- y
  Fresh (min xx yy)

-- Opgave 5

-- (\x -> \y -> if x then (y,y) else (y,y+y))  :: Num a => Bool -> a -> (a, a)

-- (\x -> \y -> \(u,v) -> (x,[y,u,v])) :: a1 -> a2 -> (a2, a2) -> (a1, [a2])

-- (Just [ \x -> 3+x]) :: Num a => Maybe [a -> a]

-- (\x -> \y -> []) :: p1 -> p2 -> [a]

-- Opgave 6

-- 6.1

squares = squaresfrom 1
  where
    squaresfrom n = n * n : (squaresfrom (n + 1))

-- 6.2

squares' = map (\x -> x * x) [1 ..]

-- 6.3

squares'' = zipWith (*) [1 ..] [1 ..]
