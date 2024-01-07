import Parsing

{- Prep exercise 1:
Use the Parser monad to define a parser theonion that can parse strings. Whenever a string is of the required
form, the parser must then give us the corresponding value of type Onion.
As an example, if we give the parser the string LLLLL7, we should get (Layer (Layer (Layer (Layer (Layer
(Core 7)))))-}
data Onion = Core Integer | Layer Onion deriving Show

core = do
  x <- int
  return (Core (toInteger x))

layer :: Parser Onion
layer = do
    char 'L'
    x <- layer
    return (Layer x)
    <|> core

theonion = parse layer

{-Prep exercise 2:
The language L = {anbn | n ≥ 0} is a well-known example of a context-language that is not also a regular
language. For instance, abba ∈ L but aab /∈ L. L can be defined using the context-free grammar
S → aSb | ε
Use the Parser monad to define a parser ab that recognizes the language L.-}
s = do
    char 'a'
    x <- s
    char 'b'
    return ("a" ++ x ++ "b")
    <|> do return ""
ab = parse s 


{-Exercise 1
Why not simplify the existing implementation of a parser for arithmetic expressions by using a
revised definition of expr?
We could write the following instead of what Graham Hutton writes for expr:
expr :: Parser Int
expr = do
    t <- term
    return t
    <|>
    do
    t <- term
    symbol "+"
    e <- expr
    return(t+e)
-}

--Den vil altid matche på første term, fordi det næste den tjekker efter er også bare et et term
--Så enten matcher den på et term eller også fejler den


{-Exercise 2 -}
term = do
    char 'a'
    return True
    <|> do 
        char 'b'
        return True

parentheses = do
    char '('
    regex
    char ')'
    return True

union = do
    char '+'
    regex
    regex
    return True

concatination = do
    char 'O'
    regex
    regex
    return True

appearsMultipleTimes = do
    char '*'
    regex
    return True

regex = do
    term
    <|>
    parentheses
    <|>
    union
    <|>
    concatination
    <|>
    appearsMultipleTimes
