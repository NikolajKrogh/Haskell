{-Prep 1-}
nsonly :: (Enum a, Num a) => a -> [a]
nsonly x = [a * x | a <- [0 ..]]

nsonly' :: (Num a) => a -> [a]
nsonly' x = generate x
  where
    generate n = n : generate (n + x)

{-Prep 2-}
-- After it evaluate it will have the tuple (17, 484001), but we take the first which 17 and thus the result is 17
plip = fst (17, f 484000)
  where
    f x = f x + 1

{-1-}
-- map iterate over the list and it adds 1 to the current number such that the kist will be [1,2,3,..]
x = 1 : (map (1 +) x)

-- take 5 x
-- = take 5 (1: map (+1) x )                 [1]
-- = take 5 (1:(2: map (+1) x ))             [1,2]
-- = take 5 (1:(2:(3: map (+1) x )))         [1,2,3]
-- = take 5 (1:(2:(3:(4: map (+1) x ))))     [1,2,3,4]
-- = take 5 (1:(2:(3:(4:(5: map (+1) x ))))  [1,2,3,4,5]

{-2-}
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

fibsfrom n1 n2 = n1 : n2 : fibstep n1 n2
  where
    fibstep n1 n2 = n1 + n2 : fibstep n2 (n1 + n2)

fib50 x = last (take x (fibsfrom 1 1))

{-3-}
indflet :: a -> [a] -> [a]
indflet _ [] = []
indflet _ [x] = [x]
indflet e (x : y : ys) = x : e : indflet e (y : ys)

-- In Haskell, undefined is a special value that represents a computation that fails with an error or does not terminate. When a Haskell program tries to evaluate undefined, it throws a runtime exception.
indfletTest = head (indflet 1 (2 : undefined))

{-4-}
allBinaries :: [String]
allBinaries = "" : [digit : rest | rest <- allBinaries, digit <- "01"]

{-
Start with "".
Prepend '0' and '1' to "", generating "0" and "1".
Prepend '0' and '1' to "0", generating "00" and "10".
Prepend '0' and '1' to "1", generating "01" and "11".
-}
{-a-}
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys

callZip = myZipWith (+) [1, 2, 3] [100, 200, 300]

fibonacci = 0 : 1 : myZipWith (+) fibonacci (tail fibonacci)

{-
fibonacci starts as [0, 1].
tail fibonacci is [1].
myZipWith (+) fibonacci (tail fibonacci) adds together 0 and 1 to get 1, so now fibonacci is [0, 1, 1].
Next, fibonacci is [0, 1, 1] and tail fibonacci is [1, 1].
myZipWith (+) fibonacci (tail fibonacci) adds together 1 and 1 to get 2, so now fibonacci is [0, 1, 1, 2].-}

{-b-}
fletind :: a -> [a] -> [a]
fletind _ [] = []
fletind e (x : xs) = x : if null xs then [] else x : fletind e xs

fletindTest = head (fletind 1 (2 : undefined))

{-c-}
data Tree = Node Tree Tree | Leaf

data Direction = L | R deriving (Show) -- left and right

type Path = [Direction]

allFinitePaths :: Tree -> [Path]
allFinitePaths Leaf = [[]]
allFinitePaths (Node left right) =
  map (L :) (allFinitePaths left) ++ map (R :) (allFinitePaths right)

{-
map (L:) (allFinitePaths left) takes each path in the left subtree and prepends L to it,
indicating that this path goes through the left child of the current node.
Similarly, map (R:) (allFinitePaths right) takes each path in the right subtree and prepends R to it.
-}
t :: Tree
t = Node (Node Leaf Leaf) (Node Leaf (Node Leaf Leaf))

{-
      Node
     /    \
  Node    Node
 /   \    /   \
Leaf Leaf Leaf Node
               /   \
             Leaf Leaf
-}

{-d-}
--hamming::(Enum a, Num a) => [a]
--hamming = [x | x <- [1..], x==1 || x `mod` 2 == 0 || x `mod` 3 == 0 || x `mod` 5 == 0]
hamming = 1 : map (2*) hamming `merge` map (3*) hamming `merge` map (5*) hamming
  where merge (x:xs) (y:ys)
          | x < y = x : xs `merge` (y:ys)
          | x > y = y : (x:xs) `merge` ys
          | otherwise = x : xs `merge` ys
{-
If x is less than y, it returns a list that starts with x, and the rest of the list is generated by merging xs and (y:ys).
If x is greater than y, it returns a list that starts with y, and the rest of the list is generated by merging (x:xs) and ys.
If x is equal to y, it returns a list that starts with x, and the rest of the list is generated by merging xs and ys. This case also helps in removing duplicates from the sequence
-}
