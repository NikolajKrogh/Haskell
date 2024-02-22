-- Exercise 1
fourfirst xs = do
  x <- xs
  return (4, x)

-- Exercise 2
data W x = Bingo x deriving (Show)

instance Functor W where
  fmap f (Bingo x) = Bingo (f x)

instance Monad W where
  return = pure
  Bingo x >>= f = f x

instance Applicative W where
  pure x = Bingo x

  -- (<*>):: Bingo (a -> b) -> Bingo a -> Bingo b
  (Bingo f) <*> (Bingo x) = Bingo (f x)

wrapadd :: Num b => b -> W b -> W b
wrapadd x y = do
  y' <- y
  return (x + y')

-- Exercise 3
data Tree a = Leaf a | Node (Tree a) (Tree a)

ordered = Node (Node (Leaf "aha") (Leaf "dingo")) (Leaf "plop")

unordered = Node (Node (Leaf "aha") (Leaf "plop")) (Leaf "dingo")

minmax :: Ord a => Tree a -> Maybe (a, a)
minmax (Leaf x) = Just (x, x)
minmax (Node left right) =
  do
    (minLeft, maxLeft) <- minmax left
    (minRight, maxRight) <- minmax right
    return (min minLeft minRight, max maxLeft maxRight)

minorder ::Ord a => Tree a -> Maybe a
minorder (Leaf x) = Just x
minorder (Node left right) =
  do
    (minLeft, maxLeft) <- minmax left
    (minRight, maxRight) <- minmax right
    if maxLeft <= minRight
        then Just minLeft
        else Nothing

-- a)



foldM :: Monad m => ( t1 -> t2 -> m t2) -> [t1] -> t2 -> m t2
foldM f [] acc = return acc
foldM f (x:xs) acc = do
    z <- f x acc
    foldM f xs z



dingo x = do
    putStrLn (show x)
    return x

func = foldM ( \ x y -> (dingo( x+y)) ) [ 1 , 2 , 3 , 4 ] 0