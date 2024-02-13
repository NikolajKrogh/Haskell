data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

isPrime :: Int -> Bool
isPrime n = null [x | x <- [2..isqrt n], n `mod` x == 0]
    where isqrt = floor . sqrt . fromIntegral


aliquotSum :: Int -> Int
aliquotSum n = sum [x | x <- [1..n-1], n `mod` x == 0]

classify :: Int -> Maybe Classification
classify n 
    | n < 1 = Nothing
    | aliquotSum n < n || isPrime n = Just Deficient
    | aliquotSum n == n = Just Perfect
    |  aliquotSum n > n = Just Abundant