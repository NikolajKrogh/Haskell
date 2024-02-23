-- PROBLEM 1

-- 1.1
-- The code does not work because there is missing Just. When using a maybe then there should be either Just or Nothing
-- The last where clause is unnecessary because x is already in scope 
-- allAnswers f [] = Just []
-- allAnswers f (x:xs) = let fun = f x
--     in if (fun == Nothing) then Nothing
--     else x : (allAnswers f xs)
--         where x=x
    
-- 1.2
allAnswers :: (a -> b) -> [a] -> [b]
allAnswers f [] = []
allAnswers f xs = map f xs

-- 1.3
--allAnswers' :: (a1 -> Maybe a2) -> [a1] -> Maybe [a1] 
allAnswers' f [] = Just []
allAnswers' f (x:xs) = 
    case f x of 
        Nothing -> Nothing
        Just _ -> fmap (x:) (allAnswers' f xs)

-- PROBLEM 2   

-- 2.1
-- func1 :: Eq a1 => a2 -> (a1, a1) -> [a2]
func1 x (a,b) = if a == b then [x] else []

-- 2.2 
--func2 :: (t1 -> t2 -> Bool) -> Bool -> t1 -> t2 -> Bool
func2 f bool x y = f x y && f x y && bool 

-- 2.3
-- func3 :: Show a => a -> IO b -> IO b
func3 x y = do 
    print x
    y

-- 2.4
-- func4 :: (a -> b) -> a -> a -> [b]
func4 f x y = [f x, f y]

-- PROBLEM 3

-- 3.1
data Tree a = Leaf a | Branch (Tree a) (Tree a)
stringTree = Branch (Leaf "dog") (Branch (Leaf "cat") (Leaf "hamster"))

-- 3.2
minimax :: Ord a => Tree a -> (a,a)
minimax (Leaf x) = (x,x)
minimax (Branch left right) = 
    let (minL, maxL) = minimax left
        (minR, maxR) = minimax right
    in (min minL minR, max maxL maxR)

-- PROBLEM 4

-- 4.1
echo = do
    putStr "Please type a word: "
    s <- getLine
    putStrLn ("You typed " ++ s)

echo' = putStr "Please type a word: " >>= \_ -> getLine >>= \s -> putStrLn ("You typed " ++ s)

-- 4.2
seconds = do
    putStr "Please input a list of tuples"
    s <- getLine
    let pairs = read s :: [(Bool, Bool)]
    print (map snd pairs)

-- PROBLEM 6

-- 5.1
-- The alternating list is not permitted because Haskell has homogenous list and it thus not possible to mix Int and Bool

-- 5.2
data Alternating a = Ints Int (Alternating a) | Bools Bool (Alternating a) | Strings String (Alternating a) | Empty deriving Show
myalt = Ints 5 (Bools True (Ints 6 (Bools False (Ints 7 (Bools True (Empty))))))

-- 5.3

separate :: Alternating a -> ([Int], [Bool])
separate Empty = ([], [])
separate (Ints i next) =
    let (ints, bools) = separate next
    in (i:ints, bools)
separate (Bools b next) =
    let (ints, bools) = separate next
    in (ints, b:bools)

-- 5.4

infiniteAlternating = number 1
    where 
        number n = Ints n(Strings (replicate n 'a')(number (n+ 1)))

-- PROBLEM 6

-- 6.1
newtype ToPairs a = TP (a,a)

toPairsB = TP(True,True)

toPairsI :: ToPairs ((Maybe Int) -> Int)
toPairsI = TP (f,f)
    where 
        f (Just x) = 1
        f Nothing = 0

-- 6.2
instance Functor ToPairs where
    -- fmap :: (a -> b) -> f a -> f b
    -- fmap :: (a -> b) -> TP a -> TP b
    fmap f (TP(x,y)) = TP(f x , f y)

instance Applicative ToPairs where
    -- pure :: a -> Maybe a 
    -- pure :: a -> ToPairs a
    pure x = TP(x,x)
    -- (<*>) :: f (a -> b) -> f a -> f b
    -- (<*>) :: ToPairs (a -> b) -> ToPairs a -> ToPairs b
    TP(f,g) <*> TP(x,y) = TP(f x, g y) 

    