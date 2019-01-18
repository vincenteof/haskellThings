module Basic.HighOrder.Exercises.Countable where 
    
-- combine 2 infinite list    
interLeave :: [a] -> [a] -> [a]
interLeave xs ys = foldr (\(x, y) acc -> x : y: acc) [] (zip xs ys)

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