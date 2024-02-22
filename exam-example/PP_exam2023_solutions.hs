newtype WrapString a = WS (a,String) deriving Show
instance Functor WrapString where
--fmap :: (a -> b) -> f a -> f b
--fmap :: (a -> b) -> WrapString a -> WrapString b
    fmap f (WS(x,s)) = WS (f x, s)

-- Fuctor = Tillad map over datatyper

instance Applicative WrapString where
    -- pure :: a -> WrapString a
    pure x = WS (x,"horse") 
    --(<*>) :: f (a -> b) -> f a -> f b
    --(<*>) :: WrapString (a -> b) -> WrapString a -> WrapString b
    (<*>) (WS (g, str1)) (WS (x,str2)) = WS (g x, str1)
    --(<*>) (WS(g,_)) wx = fmap g wx


instance Monad WrapString where
-- return :: a -> m a
-- return :: a -> WrapString a
    return x = WS(x, "") 
    -- or return = pure
-- (>>=) :: WrapString a -> (a -> WrapString b) -> WrapString b
    WS(x,_) >>= f = f x

pairup :: WrapString a -> WrapString b -> WrapString (a,b)
pairup wx wy = do
    x <- wx
    y <- wy
    return (x,y)


    
-- return = pure












-- data Tree1 a = Leaf a | Node (Tree1 a) (Tree1 a) deriving Show

-- instance Functor Tree1 where
-- -- fmap :: (a -> b) -> Tree a -> Tree b
--     fmap f (Leaf x) = Leaf (f x)
--     fmap f (Node l r) = Node(fmap f l) (fmap f r)

-- data Tree2 a = Node2 a [Tree2 a]
-- instance Functor Tree2 where
-- -- fmap :: (a -> b) -> Tree a -> Tree b
--     fmap f (Node2 x []) = (Node2 (f x)) []
--     fmap f (Node2 x xs) = (Node2 (f x)) (map (fmap f) xs)


