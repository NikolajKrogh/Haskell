main = do
  w <- getLine
  loop ((read w) :: Int)
  where
    loop 1 = putStrLn (show 1)
    loop x = do
      putStrLn (show x)
      if even x
        then loop (x `div` 2)
        else loop (3 * x + 1)

letter = do
  w <- getLine
  each (w)
  return ()
  where
    each [] = do
      return ()
    each (x : xs) = do
      putChar x
      putChar '\n'
      each xs

letters1 = do
  s <- getLine
  sequence (map putStrLn [[x] | x <- s, [x] /= []])

letters2 = do
  s <- getLine
  sequence (map (\x -> putStr [x, '\n']) s)

-- hugorm :: IO ()

hugorm = do
  putStr "How many numbers? \n"
  x <- getLine
  list <- more (read x :: Int) []
  let s = sum list in putStr ("\nThe sum is " ++ (show s) ++ "\n")
  where
    more 0 xs = return xs
    more n xs = do
      m <- getLine
      more (n - 1) ((read m :: Int) : xs)

-- a

getInts = do
  x <- getLine
  return ((read x) :: Int)

sumInts input = do
  w <- getInts
  if (w == 0) then return input else sumInts (w + input)

-- b

whileIO ioact cond fld acc =
  do
    w <- ioact
    let x = read w
     in if (cond x)
          then return acc
          else whileIO ioact cond fld (fld acc x)

myioact = getLine

mycond x = (x == 0)

myfld y acc = y + acc

-- whileIO myioact mycond myfld k