import System.IO
-- Interactive programs
-- Tell how much characters it has a line
--strlen :: IO ()
strlen = do putStr "Enter line:"
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

--Hangman game
getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
              else
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)

hangman :: IO ()
hangman =
  do putStrLn "Think of a word:"
     word <- sgetLine
     putStrLn "Try to guess it: "
     guess word

diff :: String -> String -> String
diff xs ys =
  [if elem x ys then x else '-' | x <- xs]

guess :: String -> IO ()
guess word =
  do putStr "> "
     xs <- getLine
     if xs == word then
       putStrLn "You got it"
      else
        do putStrLn (diff word xs)
           guess word

--Exercise 1
putStr' :: String -> IO ()
putStr' [] = return ()
putStr'  (x:xs) = putChar x >> putStr' xs

--Exercise 2
putStrLn' :: String -> IO()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr xs >> putStrLn' ""

putStrLn'' :: String -> IO()
putStrLn'' [] = putChar '\n'
putStrLn'' xs = putStr xs >> putChar '\n'

putStrLn''' :: String -> IO()
putStrLn''' [] = putChar '\n'
putStrLn''' xs = putStr xs >>= \ x -> putChar '\n'

putStrLn'''' :: String -> IO()
putStrLn'''' [] = putChar '\n'
putStrLn'''' xs = putStr xs >> putStrLn "\n"


--Exercise 5
sequence_ [] = return ()
sequence_ (m:ms) = (foldl (>>) m ms) >> return ()
