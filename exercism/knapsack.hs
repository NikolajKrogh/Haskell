maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue = go
  where
    go _ [] = 0
    go limit ((w, v) : xs)
      | w > limit = go limit xs
      | otherwise = max (v + go (limit - w) xs) (go limit xs)