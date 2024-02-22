-- Exercise 2
fib 1 = 1
fib 2 = 1
fib n = fib ( n-1) + fib ( n-2)
fibsfrom x y = x : ( fibsfrom y ( x+y ) )
fiblist = fibsfrom 1 1

-- Exercise 3
-- indflet :: a -> [a] -> [a]
-- indflet a xs = 