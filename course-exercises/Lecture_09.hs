import Control.Monad (when)
{- Preliminary exercise 1 -}
hello :: IO()
-- **do** indicates that what follows is a block of actions
-- putStrLn: put a String followed by a new Line. It is an IO action
hello = do putStrLn "What is your name?"
-- assign the result of getLine to name
           name <- getLine
           putStrLn ("Hello " ++ name ++ "!")



{- Preliminary exercise 2 -}
-- sequence_ takes a list of actions and perform them in sequence. _ means that the result of the actions is discarded
-- The result is "riprap" 
mySequence :: IO()
mySequence = sequence_ [putStr "rip", putStr "rap", return()]

--sequence_ requires that all the actions have the same type in this case 'IO()' and getChar has type 'IO Char'
--mySequence2 = sequence_ [putStr "rip", putStr "rap", getChar]


{- Exercise 1 what does this code do? -}
-- It takes an Int and check if it is even or odd. If it is even it divides it by 2, otherwise it multiplies it by 3 and adds 1 and calls itself recursively
main = do
    w <- getLine
    loop (read w :: Int)
    where
        loop 1 = putStrLn(show 1)
        loop x = do
            putStrLn(show x)
            if even x
                then loop (x `div` 2)
                else loop (3*x + 1)

{- Exercise 2
Use recursion to define a Haskell value letter that is a sequence of actions which does the following:
• Receive a string
• Print out the characters of the string one by one, with each character followed by a linebreak
-}
letters = do
    w <- getLine
    loop w
    where
        loop [] = return()
        loop (x:xs) = do
            putChar x
            putChar '\n'
            loop xs

{- Exercise 3
Give another definition of letters that uses the sequence function from discussion problem 2. 
-}

letters2 :: IO()
letters2 = do
    w <- getLine
    loop w
        where
            loop [] = return()
            loop (x:xs) = do
                sequence_ [putChar x, putChar '\n']
                loop xs

letters3 :: IO()
letters3 = do
    w <- getLine
    sequence_ [putStrLn [x] | x <- w]



{- Exercise 4 
Define an action hugorm :: IO() that reads a given number of integers from the keyboard, one
per line, and then finally displays the sum of the integers. As an example, we would expect the
following:
-}

hugorm :: IO()
hugorm = do
    putStr "How many numbers would you like to hugorm? "
    stringX <- getLine 
    let x = read stringX :: Int
    let y = [ putStrLn (show val) |val<-[1..x]]
    sequence_ y
    let theSum = sum [1..x]
    putStr "The sum is "
    putStrLn (show theSum)