normalizeInput :: String -> Maybe String
normalizeInput s = Just [char | char <- s, char >= '0' && char <= '9']


-- >>> number "1 (123) 456-7890"
-- Nothing
number :: String -> Maybe String
number xs = normalizeInput xs >>= validate >>= clean 
    where validate s
            | length s == 10 = Just s
            | length s == 11 && head s == '1' = Just $ tail s
            | otherwise = Nothing
          clean s
            | null s = Nothing
            | head s `elem` "01" = Nothing
            | s !! 3 `elem` "01" = Nothing
            | length s > 10 = Nothing
            | otherwise = Just s
