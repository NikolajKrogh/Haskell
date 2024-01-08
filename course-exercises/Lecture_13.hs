{- Prep exercise 1 
Give two different definitions (one using recursion, one not using recursion) of a function nsonly that takes
as input a number n and gives us the infinite list consisting of 0n, 1n, 2n, 3n, . . . 
-}
-- nsonly :: Integer -> [Integer]
multiples n m = n * m: multiples n (m+1)
nsonly n = multiples n 0

nsonlyNonRecursive n = [n * m | m <- [0..]]
    
{- Prep exercise 2 
Here is a definition of an expression.
plip = fst ( 17, f 484000 )
    where f x = f x + 1
What is the value of plip? Explain!
-}

{- The value of plip is 17, because of lazy evaluation it looks at 17 first and after it would look at f, but this would infinetly increment 484000 -}
plip = fst ( 17, f 484000 )
    where f x = f x + 1



{- Exercise 1  -}
-- x = 1 : (map (1+) x)
-- take 5 x

-- take 5 (map (1+) [1])
-- = (1 : take 4 (map (1+) [2]))
-- = (1 : (2 : take 3 (map (1+) [3])))
-- = (1 : (2 : (3 : take 2 (map (1+) [4]))))
-- = (1 : (2 : (3 : (4 : take 1 (map (1+) [5])))))
-- = (1 : (2 : (3 : (4 : (5 : take 0 (map (1+) [6]))))))
-- = 1 : 2 : 3 : 4 : 5 : []
-- [1,2,3,4,5]

{- Exercise 2 
A long time ago we saw the function
fib 1 = 1
fib 2 = 1
fib n = fib (n−1) + fib (n−2)
and discovered that computing fib 50 was not easy. Why was that?
Now define a function fibsfrom such that fibsfrom n1 n2 computes the infinite list of Fibonacci
numbers starting with n1 and n2. Then try to compute fib 50. What happens – and why?-}


-- fibsfrom takes two arguments n1 and n2, and returns an infinite list of Fibonacci numbers starting from n1 and n2. The : operator is used to prepend an element to a list in Haskell.
fibsfrom n1 n2 = n1 : n2 : fibStep n1 n2
    where  
--fibStep generates the rest of the Fibonacci sequence. 
--The function takes two arguments n1 and n2, adds them together to get the next number in the sequence, and then recursively calls itself with n2 and n1 + n2 to generate the rest of the sequence.
        fibStep n1 n2 = n1 + n2 : fibStep n2 (n1+n2)

fibs x = last (take x (fibsfrom 1 1))
x = take 50 (fibsfrom 1 1)

{-Exercise 3
In Haskell, the value undefined is polymorphic – it has type a for every type a. One can put it
anywhere in an otherwise well-typed expression and the result is well-typed. But if one tries to
evaluate the expression, the Haskell interpreter throws the exception ”undefined”.
Here is a function called indflet .
indflet [ ] = [ ]
indflet [ x ] = [ x ]
indflete ( x : y : ys ) = x : e : indflete ( y : ys )

First try to figure out without asking the Haskell interpreter what the type of indflet is and what
the function does. Next try to figure out without asking the Haskell interpreter why an exception
is throwh when you evaluate
-}

indflet :: a -> [a] -> [a]
-- it inserts e between all elements in the list

indflet _ [] = []
indflet _ [x] = [x]
indflet e (x:y:ys) = x : e : indflet e (y:ys)
--head (indflet 1 (2 : undefined))
--We evaluate with outermost evaluation
-- We first try to evaluate head, but need to go deeper
-- We then execute , and look at pattern matching we have defined
    -- it does not match the first case -list is not empty
    -- it checks the second case (where [x] is the same as (a:[])), and in order to check it needs to evaluate the second argument (2 : undefined)
-- evaluating 2:undefined, then results in error


{- Exercise 4 
Define a function allBinaries :: [String] that wil give us the infinite ordered list of all binary
numbers, with the least significant bit first, no trailing zeros, i.e.
allBinaries = [ ”0”, ”1”, ” 01 ”, ” 11 ”, ” 001 ”, ...] 
-}

allBinaries :: [String]
allBinaries = "" : [ digit : rest | rest <- allBinaries, digit <- "01" ]

 
