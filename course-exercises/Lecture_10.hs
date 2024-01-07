{- Exercise 1
The type of unbounded trees UTree is given by
data UTree a = Node a [ UTree a ]
Define an instance of Functor for UTree.
 -}

data UTree a = Node a [ UTree a ] deriving Show

instance Functor UTree where
    --fmap :: (a -> b) -> UTree a -> UTree b
    fmap g (Node a xs) = Node (g a) (fmap (fmap g) xs)

--ghci> fmap (+1) testTree
testTree = Node 1 [ Node 2 [ Node 3 [] ] ]
testTree2 = Node 1 [ Node 3 [ Node 5 [ Node 7 [] ] ] ]

{- Exercise 2
Let r be some given type. The function type constructor ((−>)r) is defined such that f a
will be (r −> a).
Define an instance of Functor for this type constructor.
 -}
-- Kan ikke compile fordi det allerede findes i ghci
--instance Functor ((->) r) where
    --fmap :: (a->b) -> fa -> fb
    --fmap f g = (\x -> f (g x)) -- gør det samme som f . g
    --fmap f g = f . g -- kortere måde at skrive det samme på. kan også skrives som fmap = (.)

{- Exercise 3 
For the applicative functor for lists we have a definion of the ”funny star” composition
<∗> on page 160. Give an alternative recursive definition of it that uses fmap.
-}

instance Applicative [] where
    --pure :: a -> [a]
    [] <*> _ = []

    -- | This function applies each function in a list of functions to each element in a list of values.
    -- It returns a new list containing the results of all possible combinations of function application.
    -- 
    -- >>> [(+1), (*2)] <*> [1, 2, 3]
    -- [2,3,4,2,4,6]
    (g:gs) <*> xs = fmap g xs ++ (gs <*> xs)
