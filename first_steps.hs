factorial n = product [1..n]

avarage ns = sum ns `div` length ns

crazyFunction word = head [length word]

n = a `div` length xs 
    where 
      a = 10
      xs = [1,2,3,4,5]

last xs = head (drop (length xs -1 ) xs) 

myProduct [] = 1 
myProduct (x : xs) = x * myProduct xs

reverseSorted [] = []
reverseSorted (x : xs) =  reverseSorted larger ++ [x] ++ reverseSorted smaller
  where smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b >= x]
