{-
Define a Haskell datatype Aexp for arithmetic expressions with addition, multiplication, numerals
and variables. The formation rules are
E ::= n | x | E1 + E2 | E1 · E2
Assume that variables x are strings and that numerals n are integers.
-}
data Aexp = N Int | X String | Add Aexp Aexp | Multiplication Aexp Aexp deriving Show

{-
Use your Haskell datatype from the previous problem to define a function eval that can, when
given a term of type Aexp and an assignment ass of variables to numbers compute the value of the
expression. Hint: Use association lists as described on page 93 to represent assignments.
As an example, if we have the assignment [x 7 → 3, y 7 → 4], eval should tell us that the value of 2 · x + y is 10.
-}

type Assoc (k, v)
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (l,v) <- t, k == l]

eval :: Aexp -> Assoc String Integer -> Integer
eval (N n) _ = n
eval (X var) ass = find var ass
eval (Add a b) ass = (eval a ass) + (eval b ass)
eval (Multiplication a b) ass = (eval a ass) * (eval b ass)

assignment = [("x",3),("y",4)]