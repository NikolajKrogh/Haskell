{- Exercise 1.
Here is the function that computes the length of a list 
lgd [] = 0
lgd (x:xs) = 1 + lgd xs

Define it again in continuation passing style and find an approriate test case for your new definition-}

lengthOfList:: [a] -> Int
lengthOfList xs = f (xs, id)
    where
        f ([], cont) = cont 0
        f (_:xs, cont) = f(xs, \result -> cont (result+1))

{- Exercise 2
Here is the definition of the function that computes Fibonacci numbers.
fib 0 = 0
fib 1 = 1
fib n = fib (n−1) + fib (n−2)
Define it again in continuation-passing style and find an appropriate test case for your new definition.
-}

fibCont n = f (n,id)
    where
        f (0, cont) = cont 1
        f (1, cont) = cont 1
        f(n, cont) = f(n-1, \r1 -> f(n-2, \r2 -> cont (r1+r2)))


{- Exercise 3 
Find an example of slack in the Haskell type system that does not involve conditional expressions or
unreachable subexpressions and only contains function definition and a single function application.
-}

--Slack is when something is not well typed, but does not necessarily cause an run time error
f x = x + x
--The function is not well typed because it is not clear what the type of x is.

{- Exercise 4
what is the result in JS?

3 + ”2” // '32'
3 ∗ ”2” // 6
”3” ∗ ”2” // 6
3 + {} // 3[object Object]
{} + 2 // 2
{3} // 3
3+{2} // Syntax error
{3}+2 // 2

-}