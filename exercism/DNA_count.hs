import Data.Char (toUpper)
import Data.Map (Map)
import qualified Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
  | isValid xs = Right $ count xs
  | otherwise = Left xs
  where
    isValid = all (`elem` "ACGT")

count :: String -> Map Nucleotide Int
count xs = M.fromListWith (+) [(read [toUpper c], 1) | c <- xs]
