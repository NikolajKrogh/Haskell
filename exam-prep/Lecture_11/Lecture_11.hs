{-Prep 1-}
tuple :: (Monad m) => m a -> m b -> m (a, b)
-- ma >>= \a -> ...: This uses the bind operator (>>=) to apply a function to the value inside the monad ma. The function is \a -> ..., which takes the value a from ma
-- return (a, b): Inside the second function, this uses the return function to wrap the tuple (a, b) in a monad.
tuple ma mb = ma >>= \a -> mb >>= \b -> return (a, b)

tuple' ma mb = do
  a <- ma
  b <- mb
  return (a, b)

{-Prep 2-}
{-func = do
    y <- z
    s y
    return ( f y )
-}
z = Just 1

s y = Just (y * 2)

f y = Just (y + 1)

doFuncWithBind = z >>= \y -> s y >>= \_ -> return (f y)

{-1-}
-- fourfirst gives us a list of pairs and thus not just the first element in the list
fourfirst xs = do
  x <- xs
  return (4, x)

fourfirstbind xs = xs >>= \x -> return (4, x)

{-2-}
data W x = Bingo x deriving (Show)

instance Functor W where
  fmap f (Bingo x) = Bingo (f x)

instance Applicative W where
  -- pure :: a -> W a
  pure = Bingo
  Bingo function <*> Bingo value = Bingo (function value)

instance Monad W where
  Bingo x >>= f = f x

wrapadd :: (Num b) => b -> W b -> W b
wrapadd x y = do
  b <- y
  return (x + b)

h :: (Num b) => W b -> W b -> W b
h x y = do
  a <- x
  b <- y
  return (a * b)

-- h (Bingo 2) (Bingo 3)

{-3-}
data Tree a = Leaf a | Node (Tree a) (Tree a)

minmax :: (Ord a) => Tree a -> Maybe (a, a)
minmax (Leaf x) = Just (x, x)
minmax (Node l r) = do
  (minL, maxL) <- minmax l
  (minR, maxR) <- minmax r
  return (min minL minR, max maxL maxR)

minorder :: (Ord a) => Tree a -> Maybe a
minorder (Leaf x) = Just x
minorder (Node l r) = do
  (minL, maxL) <- minmax l
  (minR, maxR) <- minmax r
  if maxL <= minR then Just minL else Nothing

main = do
  let mytree1 = Node (Leaf "aha") (Node (Leaf "dingo") (Leaf "plop"))
  let mytree2 = Node (Leaf "dingo") (Node (Leaf "aha") (Leaf "plop"))
  print (minorder mytree1)
  print (minorder mytree2)

{-a-}
foldM :: (Monad m) => (t1 -> t2 -> m t2) -> [t1] -> t2 -> m t2
foldM f [] var = return var
foldM f (x:xs) var = do
  result <- f x var
  foldM f xs result

dingo x = do 
  putStrLn (show x)
  return x
hello = foldM (\x y -> ( dingo ( x+y ) ) ) [ 1 , 2 , 3 , 4 ] 0