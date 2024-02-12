import Data.Char

-- | Normalizes the input string by converting it to lowercase and removing spaces and punctuation.
normalizeInput :: String -> String
normalizeInput s = [toLower ch | ch <- s, not (isSpace ch), not (isPunctuation ch)]

-- | Calculates the size of the rectangle that can fit the normalized input string.
-- If the input string is empty, returns (1, 1).
rSize :: String -> (Int, Int)
rSize s = if null calcSize then (1, 1) else minimum calcSize
    where
        calcSize = [(r, c) | c <- [2 .. (l `div` 2)], r <- [2 .. (l `div` 2)], c - r <= 1, c >= r, c * r >= l]
        l = length s

-- | Splits a string into substrings of length n.
_splitBy :: Int -> String -> [String]
_splitBy _ [] = []
_splitBy n x = [take n x] ++ _splitBy n (drop n x)

-- | Converts a string into a rectangular shape by splitting it into rows of equal length.
-- If the length of the string is less than the required number of columns, pads the last row with spaces.
toRect :: String -> [String]
toRect s = if length splited <= c then (init splited) ++ [(_last ++ [' ' | _ <- [(length _last) .. c]])] else splited
    where
        (r, c) = rSize s
        splited = _splitBy c s
        _last = last splited

-- | Transposes the rows and columns of a rectangular shape.
encRect :: [String] -> [String]
encRect rect = [[row !! ind | row <- rect] | ind <- [0 .. (length (head rect) - 1)]]

-- | Encodes the rectangular shape into a single string by concatenating the elements of each column.
encText :: [String] -> String
encText eRect = unwords eRect

-- | Encodes the input string using the crypto square algorithm.
-- If the normalized input string has a size of (1, 1), returns the input string as is.
-- Otherwise, converts the input string into a rectangular shape, transposes it, and encodes it into a single string.
encode :: String -> String
encode xs
    | (rSize $ normalizeInput xs) == (1, 1) = normalizeInput xs
    | otherwise = encText $ encRect $ toRect $ normalizeInput xs



{-
-- | Normalize a string and only keeping digits and alphas.
-- Example:
-- >>> normalize "this is a test!"
-- WAS "thisisatest"
normalize :: String -> String
normalize xs = map toLower (filter (\char -> isAlpha char || isDigit char) xs)

-- | Calculate the number of columns for the encoding grid based on the length of the input string.
calculateColumns :: Int -> Int
calculateColumns len = ceilingSqrt len
  where
    ceilingSqrt x = let y = sqrt (fromIntegral x :: Double) in if y == fromIntegral (floor y) then floor y else floor y + 1

-- | Calculate the number of rows for the encoding grid based on the number of columns and the length of the input string.
calculateRows :: Int -> Int -> Int
calculateRows c len = if c * (c - 1) >= len then c - 1 else c

-- | The 'encoding' function implements the encoding part of the square code cipher.
encoding :: String -> Int -> Int -> String
encoding xs c r = concat [[(xs ++ repeat ' ') !! (i * c + j) | i <- [0 .. r - 1], i * c + j < len] ++ " " | j <- [0 .. c - 1]]
  where
    len = length xs

-- | Encode a string using the crypto square algorithm.
-- It takes a string 'xs' and returns the encoded string
-- Example:
-- >>> encode "this is a test"
-- "tie hss iat st"
encode :: String -> String
encode xs = encoding normalized c r
  where
    normalized = normalize xs
    len = length normalized
    c = calculateColumns len
    r = calculateRows c len
-}