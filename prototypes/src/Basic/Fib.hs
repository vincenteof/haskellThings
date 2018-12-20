module Basic.Fib where

fibStep :: Num a => (a, a) -> (a, a)
fibStep (u, v) = (v, u + v)

fibPair :: (Num a, Eq a) => a -> (a, a)
fibPair 0 = (0, 1)
fibPair n = fibStep (fibPair (n - 1))

fastFib :: (Eq b, Num b) => b -> b
fastFib n = fst (fibPair n)

fibs :: (Enum b, Eq b, Num b) => b -> [b]
fibs n = map fastFib [1..n]

-- it's more like iteration instead of previous recursion version
fibs' :: Int -> [Int]
fibs' n = take n (map fst (iterate fibStep (0, 1)))