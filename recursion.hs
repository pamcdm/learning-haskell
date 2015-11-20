--import Prelude hiding ((!!))
import Prelude hiding (elem)

-- produce a list with n identical elements

and1 :: [Bool] -> Bool
and1 [] = True
and1 (b: bs) = b && and1 bs

and2 :: [Bool] -> Bool
and2 [] = True
and2 (b : bs)
  | b = and2 bs
  | otherwise = False

and3 :: [Bool] -> Bool
and3 [] = True
and3 (b : bs)
  | b == False = False
  | otherwise = and3 bs

and5 :: [Bool] -> Bool
and5 [] = True
and5 (b : bs) = and5 bs && b

-- Exercise 5
-- Valid implementations of concat function

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (xs : xss) = xs ++ concat2 xss

concat4 :: [[a]] -> [a]
concat4 [[]] = []
concat4 (xs : xss) = xs ++ concat4 xss

-- Exercise 8

hasElem :: Eq a => a -> [a] -> Bool
hasElem _ [] = False
hasElem x (y : ys)
  | x == y = True
  | otherwise = hasElem x ys

-- Exercise 9

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys

-- Exercise 10

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where (ys, zs) = halve xs

replicateList :: Int -> a -> [a]
replicateList 0 _ = []
replicateList x n = n : replicateList (x - 1) n

-- select the nth element of a list
--(!!) :: [a] -> Int -> a
--(!!) (x:xs) 0 = x
--(!!) (x:xs) p = (!!) xs (p-1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys)
  | x == y    = True
  | otherwise = elem x ys

