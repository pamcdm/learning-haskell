import System.IO

stars :: IO ()
stars =
  do putStrLn "Fala um numero"
     number <- getLine
     putStrLn (printStars (read number :: Int))

giveMeStars :: Int -> String
giveMeStars 0 = ""
giveMeStars number = "*" ++ giveMeStars (number - 1)

printStars :: Int -> String
printStars 0 = ""
printStars number = giveMeStars number ++ "\n" ++ printStars (number -1)
