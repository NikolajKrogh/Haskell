{-a.
Try to modify the definition of qsort from the file simple.hs such that it sorts the elements in descending
order – that is,
qsort [2,5,6,3,8]
should give us
[8,6,5,3,2]
Will the function call qsort ["kpst","ding","bop","plip"] make sense? Why/why not?
-}

-- Quicksort that sorts in descending order
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = big ++ [x] ++ small
                 where small = qsort [a | a <- xs, a <= x]
                       big   = qsort [a | a <- xs, a > x]

-- It will make sense since it can take any type and it will sort them after lexicographical order

{- b.
Suppose we changed the definition of qsort from the file simple.hs such that we replaced <= to <.
What would happen then?
-}

-- It would discard duplicates since a duplicate is not bigger or smaller but equal