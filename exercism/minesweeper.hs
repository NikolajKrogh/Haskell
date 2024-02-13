-- Outer list comprehension iterates over the rows of the board.
-- Inner list comprehension iterates over the cells of the board.
-- The annotateCell function is called for each cell of the board.
-- Examples:
-- >>> annotate ["*  ", "   ", "  *"]
-- ["*1 ","121"," 1*"]

annotate :: [String] -> [String]
annotate board = [[annotateCell (x, y) | (y, _) <- row] | (x, row) <- board']
  where
    -- \| Transforms the board into a list of (x, y) coordinates with the zip function.
    board' = zip [0 ..] $ map (zip [0 ..]) board
    -- \| Annotates a cell with the number of mines around it.
    -- If the cell is a mine, it returns a mine.
    -- If the cell has no mines around it, it returns a space.
    -- Otherwise, it returns the number of mines around it.
    annotateCell (x, y)
      | cell == '*' = '*'
      | count == 0 = ' '
      | otherwise = head $ show count
      where
        -- \| getting the cell at row x and column y from the board
        cell = board !! x !! y
        -- checks whether the cell at the position (x + dx, y + dy) is a mine.
        -- If this function returns True, a () value is added to the list.
        -- If it returns False, nothing is added for that iteration.
        -- Length of the list containing the () values is the number of mines around the cell.
        count = length [() | dx <- [-1 .. 1], dy <- [-1 .. 1], isMine (x + dx, y + dy)]
        -- x >= 0 checks whether the x coordinate is no negative i.e. the cell is not outside the board.
        -- x < length board checks whether the x coordinate is less than the length of the board.
        -- y < length (head board) checks whether the y coordinate is less than the length of the first row of the board.
        -- (board !! x !! y) == '*' checks whether the cell at the position (x, y) is a mine.
        isMine (x, y) = x >= 0 && x < length board && y >= 0 && y < length (head board) && (board !! x !! y) == '*'
