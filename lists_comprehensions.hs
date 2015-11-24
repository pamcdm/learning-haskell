import Data.Char

-- List comprenhensions 
-- Construct new lists from old list with generators

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

-- Using guards

generateEvenList :: Integer -> [Integer]
generateEvenList n = [x | x <- [1..n], even x]

whereAreMyFactors :: Int -> [Int]
whereAreMyFactors n = 
	[x | x <- [1..n], n `mod` x == 0]

areYouPrime :: Int -> Bool
areYouPrime n = whereAreMyFactors n == [1,n]

-- Using zip to define pairs

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

-- And then using pairs to define sorted 

sorted :: Ord a => [a] -> Bool
sorted xs =
	and [x <= y | (x,y) <- pairs xs]

-- Homework

-- Exercise 0

sum100 = sum [x ^ 2 | x <- [1 .. 100]]

-- Exercise 1

replicate n a = [a | _ <- [1 .. n]]

-- Exercise 2

pythagoras :: Int -> [(Int, Int, Int)]
pythagoras n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x^2 + y^2 == z^2]


-- Exercise 3 

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], isPerfect x]
  where isPerfect num = sum ( init (whereAreMyFactors num)) == num

-- Exercise 5 

find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [ v | (k', v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0 .. n])
  where n = length xs - 1 

-- Exercise 6

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]

-- Exercise 7 

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c 
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = int2let ((let2int c + n) `mod` 26)

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- Exercise 12 

--riffle :: [a] -> [a] -> [a]
--riffle xs ys = [x : [y] | x <- xs, y <- ys]

-- Exercise 13

divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0

divisors :: Int -> [Int]
divisors x = [d | d <- [1 .. x], x `divides` d]
