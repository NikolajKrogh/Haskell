{-1-}
-- length of list
lgd [] = 0
lgd (x : xs) = 1 + lgd xs

lengthOfList :: [a] -> Int
lengthOfList xs = f (xs, id)
  where
    f ([], continuation) = continuation 0
    f (_ : xs, continuation) = f (xs, \result -> continuation (result + 1))

{-2-}
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

fibCont n = f (n, id)
  where
    f (0, cont) = cont 1
    f (1, cont) = cont 1
    f (n, cont) = f (n - 1, \result1 -> f (n - 2, \result2 -> cont (result1 + result2)))

{-3-}
-- Slack is when something is not well typed, but does not necessarily cause an run time error
f x = x + x

-- The function is not well typed because it is not clear what the type of x is.

{-a-}
reverseList xs = f xs id
  where
    f [] cont = cont []
    f (x : xs) cont = f xs (\result -> cont (result ++ [x]))

{-b-}
--plip inserts a value y into a sorted list at the correct position to maintain the sorted order. k is the continuation function.
plip [] y k = k [y]
plip (x : xs) y k =
  if x > y
    then k (y : x : xs)
    else plip xs y (\res -> k (x : res))