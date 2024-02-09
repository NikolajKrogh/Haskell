-- | Converts a DNA sequence to its corresponding RNA sequence.
--
-- The function 'toRNA' takes a string representing a DNA sequence and returns
-- either an 'Either' value containing the corresponding RNA sequence or an
-- 'Either' value containing the first invalid character encountered in the
-- input string.
--
-- The DNA sequence is checked to ensure that it only contains the characters
-- 'A', 'C', 'G', and 'T'. If the input sequence contains any other character,
-- the function returns an 'Either' value with the first invalid character.
--
-- The conversion rules are as follows:
--   - 'G' is converted to 'C'
--   - 'C' is converted to 'G'
--   - 'T' is converted to 'A'
--   - 'A' is converted to 'U'
--   - Any other character is left unchanged.
--
-- Examples:
--
-- >>> toRNA "ACGT"
-- Right "UGCA"
--
-- >>> toRNA "ACGX"
-- Left 'X'
--
-- >>> toRNA "ACGTACGT"
-- Right "UGCAGCAU"
--
-- >>> toRNA "ACGTACGX"
-- Left 'X'
--
toRNA :: String -> Either Char String
toRNA xs 
    | all (`elem` "ACGT") xs = Right (map toRNA' xs)
    | otherwise = Left (head (filter (`notElem` "ACGT") xs))
    where 
        toRNA' 'G' = 'C'
        toRNA' 'C' = 'G'
        toRNA' 'T' = 'A'
        toRNA' 'A' = 'U'
        toRNA' x = x




toRNA2 :: String -> Either Char String
toRNA2 = traverse fromDNA
  where
    fromDNA :: Char -> Either Char Char
    fromDNA 'G' = pure 'C'
    fromDNA 'C' = pure 'G'
    fromDNA 'T' = pure 'A'
    fromDNA 'A' = pure 'U'
    fromDNA c = Left c
