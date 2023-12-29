-- Problem 1.1:
-- When using Maybe a value should have type 'Just' or 'Nothing' and the 'Just' is missing in the else
-- The last line where x=x is redundant and does nothing


-- Problem 1.2:

allAnswers :: (a -> Maybe b) -> [a] -> Maybe [b]
allAnswers f [] = Just []
allAnswers f xs = sequence (map f xs)

-- Problem 1.3:

allAnswers' f[] = Just []
allAnswers' f(x:xs) = let fun = f x
    in if (fun == Nothing) then Nothing
    else case (allAnswers' f xs) of
        Nothing -> Nothing
        Just ys -> Just (x:ys)

-- Problem 2.1
-- a)
compare :: Eq a1 => a2 -> (a1, a1) -> [a2]
compare x (y,z) = if (y == z) then [x] else []

compare' x (y,z) = if (y /= z) then [x] else []

-- b)
compare2 :: Eq a => (a -> a -> Bool) -> Bool -> a -> a -> Bool
compare2 a b c d = if c == d then a c d else b

-- c)
display :: Show a => a -> IO b -> IO b
display x y = do
    print x
    y

-- d)
func:: (a -> b) -> a -> a -> [b]
func a b c = [a b, a c]

-- Problem 2.2

-- d) is parametric polymorphism only
-- c) is overloading (ad hoc-polymorphism) only
-- a) and b) is both forms of polymorphism

-- Problem 3.1
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

-- Problem 3.2
minimax :: Ord a => Tree a -> (a, a)
minimax (Leaf x) = (x,x)
minimax (Branch left right) = (min minLeft minRight, max maxLeft maxRight)
    where
        (minLeft, maxLeft) = minimax left
        (minRight, maxRight) = minimax right

-- let exampleTree = Branch (Leaf "dog") (Branch (Leaf "cat") (Leaf "hamster"))

-- Problem 4.1

echo = putStr "Please type a word: "
    >> getLine
    >>= \s -> putStrLn ("you typed" ++ s)

-- Problem 4.2
seconds = do
    putStr "Please give a list of pairs of truth values: "
    s <- getLine
    let pairs = read s :: [(Bool, Bool)]
    print (map snd pairs)

-- Problem 5.1

{- Lists in Haskell are homogenous meaning that they can only hold elements of the same type-}

-- Problem 5.2
data Alternating = I Int Alternating | B Bool Alternating | S String Alternating | End deriving Show

myAlt:: Alternating
myAlt = I 5 (B True (I 6 (B False (I 7 (B True End)))))

-- Problem 5.3
separate :: Alternating -> ([Int], [Bool])
separate End = ([], [])
separate (I i rest) = let (ints, bools) = separate rest in (i:ints, bools)
separate (B b rest) = let (ints, bools) = separate rest in (ints, b:bools)

-- Problem 5.4
infiniteAlternating :: Alternating
infiniteAlternating = number 1
    where
        number n = I n (S (replicate n 'a') (number (n+1)))

-- Problem 6.1
newtype ToPairs a = TP (a,a)
boolPair :: ToPairs Bool
boolPair = TP (True, True)

maybePair:: ToPairs ((Maybe Int) -> Int)
maybePair = TP ((\maybeInt -> case maybeInt of
    Nothing -> 0
    Just n -> n),
    (\maybeInt -> case maybeInt of
    Nothing -> 0
    Just n -> n))

-- Problem 6.2 
instance Functor ToPairs where
    fmap f (TP (x,y)) = TP(f x, f y)

-- Problem 6.3
instance Applicative ToPairs where
    pure x = TP (x,x)
    (TP (f,g)) <*> TP(x,y) = TP(f x, g y)