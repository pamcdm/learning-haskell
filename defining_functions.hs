import Prelude hiding ((||))
import Prelude hiding ((&&))

myHalve1 xs = (take n xs, drop (n + 1) xs)
  where n = length xs `div` 2

myHalve2 xs = splitAt (div (length xs) 2) xs

myHalve3 xs = (take n xs, drop n xs) 
  where n = length xs `div` 2

-- Createing a tail but safe when an array is empty

safeTail xs = if null xs then [] else tail xs

safeTail2 [] = []
safeTail2 (_ : xs) = xs

safeTail3 (_ : xs)
  | null xs = []
  | otherwise = tail xs

safeTail4 xs 
  | null xs = []
  | otherwise = tail xs

safeTail5 xs = tail xs
safeTail5 [] = []

safeTail6 [] = []
safeTail6 xs = tail xs

safeTail7 [x] = [x]
safeTail7 (_ : xs) = xs

safeTail8 
  = \ xs ->
    case xs of
    	[ ] -> [ ]
    	(_ : xs) -> xs


-- Define another implementations of OR

False || False = False
_ || _ = True

False || False = False
_ || _ = True


-- Define another implementations of AND

True && True = True
_ && _ = False

a && b = if a then if b then True else False else False

a && b = if not (a) then not (b) else True

a && b = if a then b else False

a && b = if b then a else False

-- Defines multiplication 

mult x y z = x * y * z

-- Defines a function that takes a number n and a list and removes an element in that position

remove n xs = take n xs ++ drop (n + 1) xs

funct :: Int -> [a] -> [a]
funct x xs = take (x + 1) xs ++ drop x xs