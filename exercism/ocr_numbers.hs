import Data.List;

font :: String -> Char
font xs = case xs of
  " _ | ||_|   " -> '0'
  "     |  |   " -> '1'
  " _  _||_    " -> '2'
  " _  _| _|   " -> '3'
  "   |_|  |   " -> '4'
  " _ |_  _|   " -> '5'
  " _ |_ |_|   " -> '6'
  " _   |  |   " -> '7'
  " _ |_||_|   " -> '8'
  " _ |_| _|   " -> '9'
  _ -> '?'

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n list = (take n list) : chunksOf n (drop n list)

groupLines :: String -> [[String]]
groupLines = chunksOf 4 . lines

groupLetters :: [String] -> [String]
groupLetters = map concat . transpose . map (chunksOf 3)

convert :: String -> String
convert xs = intercalate "," $ map (map font . groupLetters) (groupLines xs)
