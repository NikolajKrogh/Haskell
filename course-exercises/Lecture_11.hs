{- Prep Exercise 1
Define a function
tuple :: Monad m => m a -> m b -> m (a,b) using explicit (>>=) and then again, now using do-notation. 
What does the function do in the case, where the monad is Maybe? -}

{- Answer: It will return a tuple of the values inside the Maybe monads.
The >>= operator in the Maybe monad is defined to propagate Nothing values. 
If the left-hand side of >>= is Nothing, the whole expression will be Nothing, 
regardless of the right-hand side.
tuple (Just 1) (Just 2) -- return Just (1,2)
tuple (Just 1) Nothing  -- return Nothing
tuple Nothing (Just 2)  -- return Nothing -}
tuple :: Monad m => m a -> m b -> m (a,b)
tuple ma mb = ma >>= \a -> mb >>= \b -> return (a,b)
tuple' ma mb = do
  a <- ma
  b <- mb
  return (a,b)


{- Prep Exercise 2
What is the expression (that uses (>>=) that is equivalent to the following do block?
do y <- z
    s y
    return (f y)
-}

z = Just 3
s y = Just (y + 1)
f y = y * 2
--z >>= \y -> ... Takes the Maybe value z and applies a function to it if it's a Just value.
-- s y >>= \_ -> ... Takes the result y from the first operation and applies the function s to it. If s y is Just, it applies another function to it.
-- return (f,y) returns the result
funcName = z >>= \y -> s y >>= \_ -> return (f y) 


{- Exercise 1 
An influencer on Instagram has become famous for his frequent updates about the List monad.
Yesterday the influencer presented a new function called fourfirst
This function takes a list and gives us a pair (4, x) where x is the first element of the list”, the
influencer concluded.
Was the influencer right? Explain, using the definition of the List monad what the code actually does and how it does it.
-}
{-Answer:
The influencer was wrong. 
The function fourfirst takes a list and returns a list of pairs (4,x) where x is the first element of the list.
Remember in between the lines of a do block bind >>= is used. 
We use xs which is a List we apply the List monad: xs >>= f = [y | x <- xs, y <- f x] 
-}
fourfirst xs = do
  x <- xs
  return (4,x)

fourfirstbind xs = xs >>= \x -> return (4,x)

{- Exercise 2 -}
data W x = Bingo x deriving Show
instance Functor W where
  fmap f ( Bingo x ) = Bingo ( f x )
instance Applicative W where
  --pure :: a -> W a
  pure = Bingo
  --(<*>) :: W (a -> b) -> W a -> W b
  Bingo f <*> Bingo x = Bingo (f x)
instance Monad W where
  --return x = Bingo x
  Bingo x >>= f = f x

wrapadd :: Num b =>b -> W b -> W b
--wrapadd x y = do: This starts the definition of the function. x is a number and y is a wrapped number.
wrapadd x y = do
--b <- y: This line uses the <- operator to extract the number from the W monad. The extracted number is named b.
  b <- y
  return (x+b) 

h :: Num b => W b -> W b -> W b
h x y = do
  a <- x
  b <- y
  return (a*b)

{-Exercise 3-}
data Tree a = Leaf a | Node (Tree a) (Tree a)
minmax :: Ord a => Tree a -> Maybe (a, a)
minmax (Leaf x) = Just (x, x)
minmax (Node l r) = do
  (minL, maxL) <- minmax l
  (minR, maxR) <- minmax r
  return (min minL minR, max maxL maxR)

minorder :: Ord a => Tree a -> Maybe a
minorder (Leaf x) = Just x
minorder (Node l r) = do
  (minL, maxL) <- minmax l
  (minR, maxR) <- minmax r
  if maxL <= minR then Just minL else Nothing

main :: IO ()
main = do
  let tree1 = Node (Leaf "aha") (Node (Leaf "dingo") (Leaf "plop")) 
  let tree2 = Node (Leaf "aha") (Node (Leaf "plip") (Leaf "dingo")) 
  print (minorder tree1) 
  print (minorder tree2) 