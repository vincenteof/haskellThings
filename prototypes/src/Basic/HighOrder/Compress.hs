module Basic.HighOrder.Compress where 

-- compress example 
skip :: Eq a => a -> [a] -> [a]
skip x [] = [x]
skip x (y:ys)
    | x == y = y : ys
    | otherwise = x : y : ys

compress :: Eq a => [a] -> [a]
compress = foldr skip []
