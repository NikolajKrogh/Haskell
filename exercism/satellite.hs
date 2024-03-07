data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

qsort [] = []
qsort (x:xs) = small ++ [x] ++ big
                 where small = qsort [a | a <- xs, a <= x]
                       big   = qsort [a | a <- xs, a > x]

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder 
  | qsort preorder /= qsort inorder = Nothing
  | inorder /= removeDuplicates inorder = Nothing
  | null preorder = Nothing
  | otherwise = Just $ go preorder inorder
  where
    go [] [] = Leaf
    go _ [] = Leaf
    go (x:xs) ys = Branch (go leftPreOrder leftInOrder) x (go rightPreOrder rightInOrder)
      where
        leftInOrder = takeWhile (/= x) ys
        rightInOrder = tail $ dropWhile (/= x) ys
        leftPreOrder = filter (`elem` leftInOrder) xs
        rightPreOrder = filter (`elem` rightInOrder) xs
