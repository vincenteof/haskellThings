module Basic.HighOrder.Exercises.Countable where 
    
-- combine 2 infinite list, ignore error case    
interLeave :: [a] -> [a] -> [a]
interLeave xs ys = foldr (\(x, y) acc -> x : y: acc) [] (zip xs ys)

-- all possible combinations

-- interLeaveLists :: [[a]] -> [a]
-- interLeaveLists = foldr1 interLeave
-- allResults = interLeaveLists [[(x, y) | y <- [1..]] | x <- [1..]]

combinationsInRange :: Integer -> [(Integer, Integer)]
combinationsInRange n
    | n < 2 = []
    | otherwise = [(x, n - x) | x <- [1..n-1]]

allCombinations = concat [combinationsInRange x | x <- [2..]]

-- remove duplicate
removeOne :: Eq a => a -> [a] -> [a]
removeOne x [] = []
removeOne x (y:ys)
    | x == y = removeOne x ys
    | otherwise = y : removeOne x ys

removeDup :: Eq a => [a] -> [a]
removeDup = foldr (\cur acc -> 
    let rmAcc = removeOne cur acc
    in if length rmAcc == length acc 
        then cur : acc 
        else rmAcc) []