{-Prep 1-}
hello = do
  putStr "What is your name?"
  xs <- getLine
  putStrLn ("hello" ++ xs)

{-Prep 2-}
sequence1 = sequence_ [putStr "rip", putStr "rap", return ()]

-- sequence2 is wrong because they do not all have the same type and all lists in haskell must have the same type
-- sequence2 = sequence_ [putStr "rip", putStr "rap" , getChar ()]

{-1-}
-- It takes an input from the user, which it reads as an int. It will now check if it is even and if it is it will divide by two if not it will say 3*x+1. When it hits 0 it will terminate
main = do
  w <- getLine
  loop (read w :: Int)
  where
    loop 1 = putStrLn (show 1)
    loop x = do
      putStrLn (show x)
      if even x
        then loop (x `div` 2)
        else loop (3 * x + 1)

{-2-}
printChars :: String -> IO ()
printChars [] = return ()
printChars (x : xs) = do
  putChar x
  putChar '\n'
  printChars xs

letters :: IO ()
letters = do
  putStr "Enter a string that you want to convert to Chars: "
  xs <- getLine
  printChars xs

{-3-}
lettersSequence :: IO ()
lettersSequence = do
  putStr "Enter a string that you want to convert to Chars: "
  string <- getLine
  loop string
  where
    loop [] = return ()
    loop (x : xs) = do
      sequence_ [putChar x, putChar '\n']
      loop xs

{-4-}
hugorm :: IO ()
hugorm = do
  putStr "How many numbers would you like to add ? "
  string <- getLine
  let number = (read string :: Int)
  let printVal = [putStrLn (show val) | val <- [1 .. number]]
  -- execute each IO action in the printVal list in order.
  sequence_ printVal
  let sumOfNumbers = sum [1 .. number]
  putStrLn ("The sum is: " ++ show sumOfNumbers)

{-a-}
sumInts :: Integer -> IO Integer
sumInts sumSoFar = do
  putStr "Enter an Integer. '0' will give the sum and terminate: "
  intStr <- getLine
  let int = read intStr
  if int == 0
    then return sumSoFar
    else sumInts (sumSoFar + int)

{-b-}
-- getIO is an IO action that reads a value
-- condF is a function that checks the termination condition
-- foldF is a function that updates the accumulated value
-- val is the initial accumulated value.
whileIO :: IO a -> (a -> Bool) -> (b -> a -> b) -> b -> IO b
whileIO getIO condF foldF val = do
  x <- getIO
  if condF x
    then return val
    else whileIO getIO condF foldF (foldF val x)

getIO :: IO Integer
getIO = do
  putStr "Enter an Integer. '0' will give the sum and terminate: "
  intStr <- getLine
  return (read intStr)

condF :: Integer -> Bool
condF = (== 0)

foldF :: Integer -> Integer -> Integer
foldF = (+)

sumInts2 :: Integer -> IO Integer
sumInts2 = whileIO getIO condF foldF