decode :: String -> String
decode [] = []
decode xs 
    | null rest = []
    | otherwise = replicate count (head rest) ++ decode (tail rest)
    where
        (num, rest) = span isDigit xs
        count = if null num then 1 else read num
        isDigit x = x >= '0' && x <= '9'

encode :: String -> String
encode [] = []
encode (x:xs) = encode' 1 xs
    where
        encode' count [] = showCount count ++ [x]
        encode' count (y:ys) =
            if x == y
                then encode' (count+1) ys
                else showCount count ++ [x] ++ encode (y:ys)
        showCount 1 = ""
        showCount n = show n