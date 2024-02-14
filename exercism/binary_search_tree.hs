module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Empty | Branch a (BST a) (BST a) deriving (Eq, Show)

-- >>> bstLeft (Branch 2 (Branch 1 Empty Empty) (Branch 3 Empty Empty))
-- Just (Branch 1 Empty Empty)
bstLeft :: BST a -> Maybe (BST a)
bstLeft tree = case tree of 
  Empty -> Nothing
  Branch _ left _ -> Just left

-- >>> bstRight (Branch 2 (Branch 1 Empty Empty) (Branch 3 Empty Empty))
-- Just (Branch 3 Empty Empty)
bstRight :: BST a -> Maybe (BST a)
bstRight tree = case tree of 
    Empty -> Nothing
    Branch _ _ right -> Just right

bstValue :: BST a -> Maybe a
bstValue tree = case tree of
    Empty -> Nothing
    Branch value _ _ -> Just value

empty :: BST a
empty = case Empty of
    Empty -> Empty

-- >>> fromList [2, 1, 3]
-- Branch 2 (Branch 1 Empty Empty) (Branch 3 Empty Empty)
fromList :: Ord a => [a] -> BST a
fromList [] = Empty
-- foldl is a left fold, which means it processes the list from left to right.
-- flip because the first argument of insert is the value and the second is the tree
fromList xs = foldl (flip insert) Empty xs

-- | Insert a value into a binary search tree.
--   If the tree is empty, create a new branch with the given value.
--   If the value is less than or equal to the current branch value, insert it into the left subtree.
--   If the value is greater than the current branch value, insert it into the right subtree.
insert :: Ord a => a -> BST a -> BST a
insert x tree = case tree of 
    Empty -> singleton x
    Branch value left right -> if x <= value then Branch value (insert x left) right else Branch value left (insert x right)


singleton :: a -> BST a
singleton x = Branch x Empty Empty

toList :: BST a -> [a]
toList tree = case tree of
    Empty -> []
    Branch value left right -> 
        let leftList  = toList left
            rightList = toList right
        in leftList ++ [value] ++ rightList
