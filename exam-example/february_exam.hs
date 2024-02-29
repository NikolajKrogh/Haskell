-- PROBLEM 1

-- 1.1
remove :: [a] -> Int -> [a]
remove [] k = []
remove (x : xs) k
  | k <= 1 = xs
remove (x : xs) k = x : remove xs (k - 1)

-- 1.2
removals :: [a] -> [[a]]
removals [] = []
removals xs = removals' xs (length xs)
  where
    removals' xs 0 = []
    removals' xs k = [remove xs i | i <- [1 .. length xs]]

-- PROBLEM 2

-- 2.1
data Person = Famous String | Ordinary String

data Unit = Single Person | Couple (Person, Person)

data Dynasty = A Unit [Dynasty]

-- dynastyExample = A Couple (Bob, Carol)
-- 2.2

-- PROBLEM 3

-- 3.1

dwindle :: [a] -> [[a]]
dwindle [] = [[]]
dwindle [x] = [[x]]
dwindle (x : xs) = (x:xs) : dwindle xs

-- 3.2
dwindle' :: [a] -> [[a]]
dwindle' xs = [drop i xs | i <- [0 .. length xs]]

-- 3.3
dwindle'' :: [a] -> [[a]]
dwindle'' xs = foldr (\x acc -> (x : head acc) : acc) [[]] xs