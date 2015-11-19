-- Exercise 0 

e0 = [False, True, False, True]

-- Exercise 1

e1 = [[1,2], [3,4]]

-- Exercise 2

e2 = [[[1, 2, 3]], [[3, 4, 5]]]

-- Exercise 3

e3 x = x * 2

-- Exercise 4

e4 (x, y) = x

-- Exercise 5 

e5 (x, y, z) = z

-- Exercise 6

e6 x y = x * y

-- Exercise 7 

e7 (x, y) = (y, x)

-- Exercise 8 

e8 x y = (y, x)

-- Exercise 9 

e9 [x, y] = (x, True)

-- Exercise 10

e10 (x, y) = [x, y]

-- Exercise 11

e11 :: (Char, Bool)
e11 = ('\a', False)

-- Exercise 12

e12 :: [(Char, Int)]
e12 = [('a', 2)]

-- Exercise 13
e13 :: Int -> Int -> Int
e13 x y = x + y
e13 x y = x + y * y

-- Exercise 14
e14 :: ([Char], [Float])
e14 = (['h', 'o', 'l', 'a'], [1.0])

-- Exercise 15
e15 :: [a] -> [b] -> (a, b)
e15 a b = [a, b]
