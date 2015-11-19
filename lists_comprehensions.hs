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