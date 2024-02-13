rows :: Int -> [[Integer]]
rows x = take x (iterate nextRow [1])
    where
        nextRow row = zipWith (+) (0:row) (row++[0])

{-
(0:row) prepends a 0 to the start of the current row.
(row++[0]) appends a 0 to the end of the current row.
The zipWith (+) function then adds together corresponding pairs of numbers from these two lists.

For example, if the current row is [1, 3, 3, 1], the next row is calculated as follows:
zipWith (+) (0:[1, 3, 3, 1]) ([1, 3, 3, 1]++[0])
= zipWith (+) [0, 1, 3, 3, 1] [1, 3, 3, 1, 0]
= [0+1, 1+3, 3+3, 3+1, 1+0]

-}