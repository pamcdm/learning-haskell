-- exercise 1
all1 :: (a -> Bool) -> [a] -> Bool
all1 p xs = and (map p xs)

all3 :: (a -> Bool) -> [a] -> Bool
all3 p = and . map p

all4 :: (a -> Bool) -> [a] -> Bool
all4 p = not . any (not . p)

-- exercise 2
any2 :: (a -> Bool) -> [a] -> Bool
any2 p = or . map p

any3 :: (a -> Bool) -> [a] -> Bool
any3 p xs = length (filter p xs) > 0

any4 :: (a -> Bool) -> [a] -> Bool
any4 p = not . null . dropWhile ( not . p)

any5 :: (a -> Bool) -> [a] -> Bool
any5 p = null . filter p

any6 :: (a -> Bool) -> [a] -> Bool
any6 p xs = not (all (\ x -> not (p x)) xs)

any7 :: (a -> Bool) -> [a] -> Bool
any7 p xs = foldr (\ x acc -> (p x) || acc) False xs

any8 :: (a -> Bool) -> [a] -> Bool
any8 p xs = foldr (||) True (map p xs)

-- exercise 3
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 p (x : xs)
  | p x = x : takeWhile1 p xs
  | otherwise = takeWhile1 p xs

-- exercise 4
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 p (x : xs)
  | p x = dropWhile1 p xs
  | otherwise = x : xs

dropWhile2 :: (a -> Bool) -> [a] -> [a]
dropWhile2 _ [] = []
dropWhile2 p (x : xs)
  | p x = dropWhile2 p xs
  | otherwise = xs

dropWhile3 p = foldr (\ x acc -> if p x then acc else x : acc) []

-- exercise 6
filtering :: (a -> Bool) -> [a] -> [a]
filtering p = foldl (\ xs x -> if p x then x : xs else xs) []

filtering2 :: (a -> Bool) -> [a] -> [a]
filtering2 p = foldl (\ xs x -> if p x then xs ++ [x] else xs) []
