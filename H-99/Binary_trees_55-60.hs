data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

tree1 =
  Branch
    'a'
    ( Branch
        'b'
        (Branch 'd' Empty Empty)
        (Branch 'e' Empty Empty)
    )
    ( Branch
        'c'
        Empty
        ( Branch
            'f'
            (Branch 'g' Empty Empty)
            Empty
        )
    )

-- A binary tree consisting of a root node only
tree2 = Branch 'a' Empty Empty

-- An empty binary tree
tree3 = Empty

-- A tree of integers
tree4 =
  Branch
    1
    (Branch 2 Empty (Branch 4 Empty Empty))
    (Branch 2 Empty Empty)

-- 55. Construct completely balanced binary trees
-- In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.
-- Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes. The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.
-- cbalTree = completely balanced binary trees
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n =
  -- quotRem is the quotient and remainder of dividing two integers and is used to split the number of nodes in the left and right subtrees
  let (q, r) = (n - 1) `quotRem` 2
   in [ Branch 'x' left right | i <- [q .. q + r], left <- cbalTree i, right <- cbalTree (n - i - 1)
      ]

-- 56. Symmetric binary trees
-- Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Write a predicate symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.
mirror :: Tree a1 -> Tree a2 -> Bool
mirror Empty Empty = True
mirror (Branch _ a b) (Branch _ x y) = mirror a y && mirror b x
mirror _ _ = False

symmetric :: Tree a2 -> Bool
symmetric t = mirror t t

{-ghci> symmetric tree1
False
ghci> symmetric tree2
True-}

-- 57. Binary search trees (dictionaries)
-- Use the predicate add/3, to write a predicate to construct a binary search tree from a list of integer numbers.
add :: (Ord a) => a -> Tree a -> Tree a
add x Empty = Branch x Empty Empty
-- @ means "as pattern" and it's used to avoid repeating the same pattern on both sides of the equation
add x t@(Branch y l r) = case compare x y of
  -- LT + GT + EQ are defined in the Ord typeclass
  LT -> Branch y (add x l) r
  GT -> Branch y l (add x r)
  EQ -> t

construct :: (Foldable t, Ord a) => t a -> Tree a
construct xs = foldl (flip add) Empty xs

constructTest = construct [5, 3, 18, 1, 4, 12, 21]

constructAndSymetricTest = symmetric (construct [5, 3, 18, 1, 4, 12, 21])

-- 58. Generate-and-test paradigm
-- Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.
-- symCbalTrees = symmetric completely balanced trees
symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree

symCbalTreesTest = symCbalTrees 5

-- 59. Construct height-balanced binary trees
-- In a height-balanced binary tree, the following property holds for every node:
-- The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.
-- Construct a list of all height-balanced binary trees with the given element and the given maximum height.
heightBalancedTree :: a -> Int -> [Tree a]
heightBalancedTree x 0 = [Empty]
heightBalancedTree x 1 = [Branch x Empty Empty]
heightBalancedTree x h =
  [ Branch x l r
    | (hl, hr) <- [(h - 2, h - 1), (h - 1, h - 1), (h - 1, h - 2)],
      l <- heightBalancedTree x hl,
      r <- heightBalancedTree x hr
  ]

heightBalancedTreeTest = heightBalancedTree 'x' 2

-- 60. Construct height-balanced binary trees with a given number of nodes
-- Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain?

-- maximum number of nodes in a weight-balanced tree of height h
maxNodes :: Int -> Int
maxNodes h = 2 ^ h - 1

-- minimum height of a weight-balanced tree of n nodes
minHeight :: Int -> Int
minHeight n = ceiling $ logBase 2 $ fromIntegral (n + 1)

-- minimum number of nodes in a weight-balanced tree of height h
minNodes :: Int -> Int
minNodes h = fibs !! (h + 2) - 1

-- maximum height of a weight-balanced tree of n nodes
maxHeight :: Int -> Int
maxHeight n = length (takeWhile (<= n + 1) fibs) - 3

-- Fibonacci numbers
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

heightBalancedTreeNodes :: a -> Int -> [Tree a]
heightBalancedTreeNodes x n = [t | h <- [minHeight n .. maxHeight n], t <- baltree h n]
  where
    -- baltree h n = weight-balanced trees of height h with n nodes
    -- assuming minNodes h <= n <= maxNodes h
    baltree 0 n = [Empty]
    baltree 1 n = [Branch x Empty Empty]
    baltree h n =
      [ Branch x l r
        | (hl, hr) <- [(h - 2, h - 1), (h - 1, h - 1), (h - 1, h - 2)],
          let min_nl = max (minNodes hl) (n - 1 - maxNodes hr),
          let max_nl = min (maxNodes hl) (n - 1 - minNodes hr),
          nl <- [min_nl .. max_nl],
          let nr = n - 1 - nl,
          l <- baltree hl nl,
          r <- baltree hr nr
      ]

heightBalancedTreeNodesTest = heightBalancedTreeNodes 'x' 4