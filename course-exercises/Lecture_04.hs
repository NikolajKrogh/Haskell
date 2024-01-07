-- Define, using pattern matching and without using the length function, a function onlytwo that tells
-- us if a list has precisely two elements – in which case it must return True – or not, in which case it
-- must return False. What is the type of onlytwo?

-- The type of onlytwo is onlytwo :: [a] -> Bool
onlytwo :: [a] -> Bool
onlytwo [] = False
onlytwo [x] = False
onlytwo [_,_] = True
onlytwo _ = False

{-
The dot product of two pairs of numbers (a, b) and (c, d) is the number a · c + b · d. Define, using
list comprehension, a function alldots that takes two lists of pairs of numbers and returns all the
possible dot products of every pair from the first list and every pair from the second list. Find two
good test case for testing your function definition and use them to test your code. What is the type
of alldots ?
-}

alldots :: [(Int, Int)] -> [(Int, Int)] -> [Int]
alldots xs ys = [a*c + b*d | (a,b) <- xs, (c,d) <- ys]


{-
Use list comprehension to define a function sevens that given an integer k gives us a list of all
natural numbers that are divisible by 7 and are less than k. First find out what its type should be.
-}
seven :: Int -> [Int]
seven k = [x | x <- [1..k - 1], x `mod` 7 == 0]

{-
A Pythagorean triple is a triple (a, b, c) of natural numbers a, b, and c, such that a ≤ b < c and
a^2 + b^2 = c^2. In other words, a triple of this form gives us the length of the three sides of a right
triangle for which all sides have integer length. The smallest Pythagorean triple is (3, 4, 5).

Use list comprehension to define a function pyt that, when given an integer k, gives us a list of all
Pythagorean triples whose largest element is at most k. Before you write the definition of pyt, find
out what its type should be.
-}

pyt :: Int -> [(Int, Int, Int)]
pyt k = [(a,b,c) | a <- [1..k], b <- [1..k], c <- [1..k], a^2 + b^2 == c^2]


{-
3. (Everyone at the table together – 10 minutes)
During breaks in the recording of Married at first sight one of the couples decided to write a function
headsup that can tell us if the two first elements of a list are identical. Here is what the couple
wrote.
headsup x = i f head x == head ( t a i l x ) then True e l s e F a l s e
The couple felt that the type of headsup ought to be
[Num] −> Bool
-}

--Instead of [Num] −> Bool it should just be a generic type, such that it can be used on any type of list.


{-
Show how the meaning of the following curried function definition can be given in terms of lambda
expressions from Haskell.
plonk x y z = x+y+z
Figure out the type of plonk without asking the Haskell interpreter.
-}
plonk :: Int -> Int -> Int -> Int
plonk = \x -> (\y -> (\z -> x + y + z))



{-
Find a Haskell expression whose type is (Ord a1, Eq a2) =>a2 −> a2 −> (a1, a1)−> a1
-}

expression b c (a1,a2) = if (a1<a2) then a1 else if (b==c) then a1 else a2
