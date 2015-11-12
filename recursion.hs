-- produce a list with n identical elements

replicateList :: Int -> a -> [a]
replicateList 0 = []
replicateList x = x : replicateList (x - 1)