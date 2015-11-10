myHalve1 xs = (take n xs, drop (n + 1) xs)
  where n = length xs `div` 2

myHalve2 xs = splitAt (div (length xs) 2) xs

myHalve3 xs = (take n xs, drop n xs) 
  where n = length xs `div` 2

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

