import Parsing

{-Prep 1-}
data Onion = Core Integer | Layer Onion deriving (Show)

core :: Parser Onion
core = do
  n <- integer
  return (Core n)

layer :: Parser Onion
layer = do
  char 'L'
  onion <- theonion
  return (Layer onion)

theonion :: Parser Onion
theonion = layer <|> core
