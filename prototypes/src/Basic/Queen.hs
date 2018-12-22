module Basic.Queen where 

import Data.List (permutations)

-- return: the num of ways of putting `qNum` queens to a `qNum` * `size` board    
positions :: Int -> Int -> [[Int]]
positions 0 _ = [[]]
positions qNum size = [pos : layout | pos <- [1..size], layout <- positions (qNum - 1) size]

notSameRow :: [Int] -> Bool
notSameRow [] = True
notSameRow (x:xs) = notElem x xs && notSameRow xs

notSameDiag :: [Int] -> Bool
notSameDiag [] = True
notSameDiag xs@(x:xs') = and [abs (i1 - i) /= abs (p1 - p) | (i, p) <- ip] && notSameDiag xs'
    where (i1, p1):ip = zip [1..] xs

queen :: Int -> [[Int]]
queen n = [xs | xs <- positions n n, notSameRow xs, notSameDiag xs]

positions' :: Int -> Int -> [[Int]]
positions' 0 _ = [[]]
positions' qNum n = 
    [x : xs | xs <- positions' (qNum - 1) n, x <- [1 .. n], isSafe x xs]

isSafe :: Int -> [Int] -> Bool
isSafe y ys = not (elem y ys || sameDiag y ys)
    where sameDiag y ys = any (\(dist, y') -> abs (y' - y) == dist) $ zip [1 ..] ys

queen8' = positions' 8 8

queen' n = [xs | xs <- permutations [1 .. n], notSameDiag xs]