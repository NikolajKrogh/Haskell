-- | 'cleanInput' removes the "What is " prefix, the "?" suffix, and the "by" words from the input string,
-- | and splits the remaining string into words.
cleanInput :: String -> [String]
cleanInput problem = reverse $ filter (/= "by") $ words $ init $ drop 7 problem

-- | 'parse' recursively parses a list of words into an integer.
-- | It expects the list to be int the form "2 3 plus" instead of "2 plus 3".
parse :: [String] -> Maybe Integer
parse [] = Nothing
parse [value] = toInt value
parse (value:op:rest) = case (toInt value, toOp op, parse rest) of
    (Just x, Just op', Just y) -> Just $ op' y x
    _ -> Nothing

-- | 'toInt' tries to convert a string to an integer.
-- | If the string cannot be parsed as an integer, it returns Nothing.
toInt :: String -> Maybe Integer
toInt x = Just (read x :: Integer)

-- | 'toOp' converts a string to a binary operation.
-- | If the string does not represent a recognized operation, it returns Nothing.
toOp :: String -> Maybe (Integer -> Integer -> Integer)
toOp op = case op of
    "plus" -> Just (+)
    "minus" -> Just (-)
    "multiplied" -> Just (*)
    "divided" -> Just div
    _ -> Nothing

-- | 'answer' takes a problem string, cleans it, and parses it into an integer.
answer :: String -> Maybe Integer
answer problem = parse (cleanInput problem)