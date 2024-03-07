score :: Float -> Float -> Int
score x y
    | d > 10 = 0
    | d > 5  = 1
    | d > 1  = 5
    | otherwise = 10
    -- Calculate the cartesian distance from the origin
    where d = sqrt (x*x + y*y)