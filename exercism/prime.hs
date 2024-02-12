isPrime :: Integer -> Bool
isPrime n = null [x | x <- [2..isqrt n], n `mod` x == 0]
    where isqrt = floor . sqrt . fromIntegral

primes :: [Integer]
primes = filter isPrime [2..]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

nth :: Int -> Maybe Integer
nth n
    | n <= 0 = Nothing
    | otherwise = listToMaybe $ drop (n-1) primes


nth2 :: Int -> Maybe Integer
nth2 n 
    | n > 0 = Just $ primes2 !! (n - 1)
    | otherwise = Nothing

primes2 :: [Integer]
primes2 = sieve [2..]
    where
        sieve [] = []
        sieve (p:ps) = p : sieve [x | x <- ps, mod x p /= 0]
       
