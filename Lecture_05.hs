{- 
1. Define the function replicate – and use pattern matching in your solution. This function takes an
integer n and and an element x and gives us a list with n elements where x has been repeated
exactly n times. As an example, replicate 3 5 should give us [5,5,5] . What should the type of
replicate be?
-}

replicateList :: Int -> a -> [a]
replicateList 0 _ = []
replicateList n x = x : replicateList (n-1) x


{-
Define the function improve – and use pattern matching in your solution. It takes a list xs and, if
xs contains at least two elements, it gives us a list where every other element has been removed.
As an example, improve [1,2,3,4,5,6,7] should give us [1,3,5,7] . What should the type of improve
be?
-}

improve :: [a] -> [a]
improve [] = []
improve [x] = [x]  
{-
Initial call: improve [1, 2, 3, 4, 5, 6, 7]
Recursive call: improve [3, 4, 5, 6, 7]
Recursive call: improve [5, 6, 7]
Recursive call: improve [7]
Gives the list [1, 3, 5, 7]
-}
improve (first:second:rest_of_list) = first : improve rest_of_list



{-
The function reverse appears in the Haskell prelude. It will reverse a list such that e.g. reverse
[1,2,3] evaluates to [3,2,1] .
Now it is your task to define your own version of this function, rev. First try to find out what the
type of rev should be and follow the overall approach described in Section 6.6
-}

reverseList::[a]->[a]
reverseList [] = []
reverseList [x] = [x]
reverseList (x : xs) = reverseList xs ++ [x]



{-
In an earlier session we used other functions from the prelude to define the function last that finds
the last element of a list.
Now it is your task to give a recursive definition of mylast (which you should call it, since last
exists in the prelude). First try to find out what the type of mylast should be and follow the overall
approach described in Section 6.6.
-}

myLast :: [a] -> a
myLast [a] = a
myLast (x : xs) = myLast xs




{-
The function isolate takes a list l and an element x and returns a pair of two new lists (l1 , l2).
The first list l1 is a list that contains all elements in l that are not equal to x. The second list l2
is a list that contains all occurrences of x in l.
• isolate [4,5,4,6,7,4] 4 evaluates to ([5,6,7],[4,4,4]) .
• isolate [’ g ’,’ a ’,’ k ’,’ a ’] ’a’ evaluates to ([’ g ’,’ k ’], [’ a ’,’ a ’]) .
Define isolate in Haskell. What should the type of isolate be? Major hint: Place the recursive
call in a let - or where-clause and use pattern matching to find the components in the result of that
call.
-}



isolate :: (Eq x) => [x] -> x -> ([x], [x])
isolate [] _ = ([], [])
isolate (x : xs) y
  | x == y = (fst (isolate xs y), x : snd (isolate xs y))
  | otherwise = (x : fst (isolate xs y), snd (isolate xs y))


{-
The function wrapup is a function that takes a list and returns
a list of lists. Each list in this list contains the successive elements from the original list that are
identical.
For instance, wrapup [1,1,1,2,3,3,2] should give us the list [[1,1,1],[2],[3,3],[2]]
-}

wrapup :: (Eq x) => [x] -> [[x]]
wrapup [] = []
wrapup (x : xs) = l1 : l2
  where
    l1 = x : takeWhile (== x) xs
    l2 = wrapup (dropWhile (== x) xs)



