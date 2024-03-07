data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

first (a, _, _) = a
second (_, a, _) = a
third (_, _, a) = a

label :: Resistor -> String
label resistor 
  | ohms resistor < 1000 = show (ohms resistor) ++ " ohms"
  | ohms resistor < 1000000 = show (ohms resistor `div` 1000) ++ " kiloohms"
  | ohms resistor < 1000000000 = show (ohms resistor `div` 1000000) ++ " megaohms"
  | otherwise = show (ohms resistor `div` 1000000000) ++ " gigaohms"

ohms :: Resistor -> Int
ohms resistor = (10 * fromEnum (first (bands resistor)) + fromEnum (second (bands resistor))) * (10 ^ fromEnum (third (bands resistor)))