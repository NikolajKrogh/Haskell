boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
-- 
boardString white black = unlines $ map unwords board
  where
    board = [[cell (rows, columns) | columns <- [0 .. 7]] | rows <- [0 .. 7]]
    cell position
      | white /= Nothing && Just position == white = "W"
      | black /= Nothing && Just position == black = "B"
      | otherwise = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack queenA queenB = sameRow || sameColumn || sameDiagonal
  where
    sameRow = fst queenA == fst queenB
    sameColumn = snd queenA == snd queenB
    sameDiagonal =  abs (fst queenA - fst queenB) == abs (snd queenA - snd queenB)