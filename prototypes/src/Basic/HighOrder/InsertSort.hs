module Basic.HighOrder.InsertSort where

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x < y = x : y : ys
    | otherwise = y : insert x ys

insertSort :: Ord a => [a] -> [a]
insertSort = foldr insert []