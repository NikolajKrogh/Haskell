{-Prep 1.-}

quango:: a -> [a]
quango x = [x]
-- quango is parametric polymorphic because it can handle values identically without depending on their type

tango:: Num p1 => (a,b) -> p2 -> p1
tango (x,y) z = 42
--Tango is ad hoc polymorphic because it allows for different implementation based on its type of argument

{-1-}
--The function twice takes two arguments: a function f and a value x. 
--It applies the function f to the value x twice. 
--Therefore, the function f should have a type of a -> a, meaning it takes an argument of type a and returns a value of the same type a.
twice:: (a -> a) -> a -> a
twice f x = f (f(x))

twicetwo:: (a -> a,a) -> a
twicetwo (f,x) = f (f(x))

{-3 Lambda calculus I am not going to do-}

{-2-}
--dingo is parametric polymorphic
dingo:: (a, a) -> [a]
dingo (x,y) = [x,y]

{-4-}
bighead:: Ord a => [a] -> Int
bighead (x:xs) = length (filter(> x) xs)

{-a-}
mango:: Int -> (Int -> (Int -> Int))
mango x y z = x * y + z - 42

{-b-}
--bingo is parametric polymorphic
bingo:: a -> a
bingo a = a

{-c-}
thesame::Eq a => [(a,a)] -> [(a,a)]
thesame xs = filter(uncurry (==)) xs 

{-d-}
list1:: Num a => [a->a->a]
list1 = [(+),(*),(+),(-)]

--list2 = [(+),(*),(+),(-),(++)] This is not possible because (++) is list concatenation which works with list not numbers

{-e-}
myDouble::Num a => [a] -> [a]
myDouble xs = map (*2) xs

{-f-}
hello::(Ord a1, Eq a2) => a2 -> a2 -> (a1, a1) -> a1
hello x y (z1, z2) 
    | x == y = z1
    | otherwise = z2

{-g-}
madras (f,x,y) = f (f x x) y

--It does the same as madras, but it now take the arguments separtely
madrasCurried :: (t -> t -> t) -> t -> t -> t
madrasCurried f x y = f (f x x) y