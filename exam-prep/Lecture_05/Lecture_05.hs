{-Prep 1-}
replicate':: Int -> a -> [a]
replicate' 0 _ = []
replicate' x y = y : replicate' (x-1) y
replicate'' x y 
    | x <= 0 = []
    | otherwise = y : replicate'' (x-1) y

{-Prep 2-}
improve:: [a] -> [a]
improve [] = []
improve [x] = [x]
improve (x:xs:xss) = x: improve xss

{-1-}
reverse':: [a] -> [a]
reverse'[] = []
reverse' (x:xs) = reverse' xs ++ [x]

{-2-}
mylast::[a] -> [a]
mylast [] = []
mylast [x] = [x]
mylast (x:xs) = mylast xs

{-3-}
isolate::Eq a => [a] -> a -> ([a],[a])
isolate [] b = ([],[])
isolate (x:xs) b
    | x == b = (fst(isolate xs b), x : snd(isolate xs b))
    | otherwise = (x : fst (isolate xs b), snd (isolate xs b))

{-4-}
wrapup:: Eq a => [a] -> [[a]]
wrapup [] = []
wrapup [x] = [[x]]
wrapup (x:xs) = l1 : l2 
    where
        l1 = x : takeWhile (== x) xs
        l2 = wrapup (dropWhile (==x) xs)


wrapup' :: Eq a => [a] -> [[a]]
wrapup' [] = []
wrapup' [x] = [[x]]
wrapup'(x:xs) = 
    if x == h
        then (x:fst):rst
        else [x] : (fst:rst)
    where 
        fst:rst = wrapup' xs
        (h:t) = fst

{-5-}
triples:: Num a => [(a,a,a)] -> ([a],[a],[a])
triples [] = ([],[],[])
triples [(a,b,c)] = ([a],[b],[c])
triples ((x,y,z):xs) = (x:xs', y:ys', z:zs')
    where (xs', ys', zs') = triples xs

{-a-}
--run-length encoding
rle :: Eq a => [a] -> [(a, Int)]
rle [] = []
rle (x:xs) = rle' (x, 1) xs
    where
        rle' (x, n) [] = [(x, n)]
        rle' (x, n) (y:ys) =
            if x == y
                then rle' (x, n+1) ys
                else (x, n) : rle' (y, 1) ys
{-b-}
amy:: Integral a => [a] -> Bool
amy [] = False
amy (x:xs) = odd x || amy xs

amy':: Integral a => [a] -> Bool
amy' = any odd

{-c-}
frequencies :: String -> [(Char, Int)]
frequencies [] = []
--x er det første element i listen. Det andet element i tuplen er hvor gange x optræder og det tæller vi ved at lave en ny liste med kun det element og tage længden af det.
-- Vi kalder igen frequencies og fjerner x fra listen
frequencies (x:xs) = (x, length (x : filter (== x) xs)) : frequencies (filter (/= x) xs)