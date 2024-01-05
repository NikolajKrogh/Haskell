{-1-}
func1 :: (Ord a, Num a) => a -> a -> [[Bool]] -> Bool
func1 x y boolLists = (x > y) && null boolLists

func2 :: (Num a) => (t -> a, t) -> a -> a
func2 (f, x) y = f x + y

func3 :: (Fractional t1) => (t2 -> t1) -> (t2 -> t1) -> (t1 -> t3) -> t2 -> t3
func3 f g h x = h (f x / g x)

{-3-}
triples :: (Num a) => [(a, a, a)] -> ([a], [a], [a])
triples [] = ([], [], [])
triples [(a, b, c)] = ([a], [b], [c])
triples ((x, y, z) : xs) = (x : xs', y : ys', z : zs')
  where
    (xs', ys', zs') = triples xs

{-4-}
cfrac :: Double -> Int -> [Int]
cfrac _ 0 = []
cfrac r n = a : cfrac (1 / f) (n - 1)
  where
    a = floor r
    f = r - fromIntegral a

{-5-}
class InVector v where
  (&&&) :: v -> v -> v
  (***) :: v -> v -> Int

instance InVector Bool where
  (&&&) = (&&)
  x *** y = if x && y then 1 else 0

{-a-}
frequencies :: String -> [(Char, Int)]
frequencies [] = []
frequencies (x : xs) = (x, length (x : filter (== x) xs)) : frequencies (filter (/= x) xs)