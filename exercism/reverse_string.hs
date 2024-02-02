reverseString :: String -> String
reverseString str = reverse str


-- Recursive solution
reverseSentence :: String -> String
reverseSentence [] = []
reverseSentence (x:xs) = reverseSentence xs ++ [x]