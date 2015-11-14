-- produce a list with n identical elements

replicateList :: Int -> a -> [a]
replicateList 0 = []
replicateList x = x : replicateList (x - 1)

-- select the nth element of a list
selectNElement :: [a] -> Int -> a
selectNElement [x] = x
selectNElement xs = tail (selectNElement -1)