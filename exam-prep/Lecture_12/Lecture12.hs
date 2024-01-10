import Parsing

-- Formation rules:
-- R ::= a | b | R1 ◦ R2 | R1 ∪ R2 | R* | (R_1)
-- These formation rules do not show how each input should be parsed e.g. what output it should return

value =
  do
    some (char 'a')
    <|> some (char 'b')
    <|> error "Invalid char"

concatn = do
  x <- regex
  char 'o'
  y <- regex
  return x

union = do
  x <- regex
  char 'U'
  y <- regex
  return x <|> return y

paranth =
  do
    char '('
    x <- regex
    char ')'
    return x
    <|> value

regex =
  do
    paranth
    <|> union
    <|> concatn
    <|> value

-- regexp = do
--  do
--    char "a"
--    <|> char "b"
--  do
--    char "b"
--    many (char "c")
--    return "Ok"