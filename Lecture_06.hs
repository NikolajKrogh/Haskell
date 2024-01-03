{-
Every letter in the lowercase English alphabet has a position. ”a” has position 1, ”c” has
position 3 and ”h” has position 8.
In Haskell, every string is a list of characters. So String is the same type as [Char].
We can define a function positions that, given a string of lowercase letters str gives us the
list of positions of the characters in str.
As an example, positions ”abba’’ gives us [1,2,2,1] . Use the higher-order functions in
Chapter 7 to define positions.
-}
position:: (Char -> Int) -> [Char] -> [Int]
position f [] = []
position f (x:xs) = (f x-96) : position f xs

{-
Exercise 1
The within function takes a list of numbers and a pair of numbers, returns a list of numbers which
are in the input list and within the range (inclusive) given by the input pair.
The elements in the output list appear be in the same order they appeared in the input list. If the
input pair is (n1,n2), assume that n1 is the lower bound of the range and n2 is the upper bound of
the range.
As an example, within [1,3,4,5,2] (1,3) should give us [1,3,2] and within [1,3,4,5,2] (3,1) should
give us [] .
Define within using the higher-order functions in Chapter 7.
-}

--within::(Num a Ord a) => [a] -> (a, a) -> [a]
--within [] f = []

within xs (a,b) = filter withinab xs
    where 
        withinab x = a<=x && x<=b


{- 
Exercise 2
Implement the sumrows function. The function takes a list of number lists returns a one-dimensional
list of numbers with each number equal to the sum of the corresponding row in the input list. If a
list is empty, its sum is 0.
As an example, sumrows [[1,2], [3,4]] should give us [3, 7], and sumrows [[],[],[1]] should give
us [0,0,1] .
Define sumrows using the higher-order functions in Chapter 7.
-}
sumrows:: (Num a) => [[a]] -> [a]
sumrows xss = map sum xss

{-
Exercise 3
The base of the natural exponential function e = 2.718
The function approx should give us the approximation of e that we find by adding the first n terms
of this infinite series
Define approx using the higher-order functions in Chapter 7
-}

factorial:: (Num a, Enum a) => a -> a
factorial n = foldr (*) 1 [1..n]

approx:: (Fractional a, Enum a) => a -> a
approx x = sum(map (\a -> 1/factorial a) [0..x])

{-
Exercise 4
Here is a function. What does it do? And why? Use the explanation at the very end of Section 7.3
in the book to help you answer the ”why”-question.
fingo:: [a] −> [a] −> [a]
fingo xs ys = foldr (:) xs ys
-}

fingo:: [a] -> [a] -> [a]
fingo xs ys = foldr (:) xs ys

[1,2,3] [4,5,6]
 
1(2:(3:[]))
4(5:(6:[1,2,3]))

[4,5,6,1,2,3]


{-
Exercise 5
The function map can be applied to any function, so we can write map map. What is the type of
map map? Figure this out without asking the Haskell interpreter – try to justify your answer and
only then ask the interpreter.
-}
mapmap:: [a -> b] -> [[a] -> [b]]
mapmap = map map


