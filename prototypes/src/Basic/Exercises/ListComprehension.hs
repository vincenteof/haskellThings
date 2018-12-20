module Basic.Exercises.ListComprehension where

import Data.List (group)    

factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], mod n x == 0]

isPrime :: Integral a => a -> Bool
isPrime n = factors n == [1,n]

primes :: Integral a => a -> [a]
primes n = [x | x <- [1..n], isPrime x]

isPrime' :: Integral a => a -> Bool
isPrime' p = odd p && all (\n -> p `mod` n /= 0) (takeWhile (\n -> n * n <= p) [3, 5..])

nextPrime :: Integer -> Integer
nextPrime n
    | odd n = if isPrime' n then n else nextPrime (n + 2)
    | otherwise = nextPrime (n + 1)

sieve :: (Integral a) => [a] -> [a]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primes' = sieve [2..]

primeFactors :: (Integral a, Eq a) => a -> [(Int, a)]
primeFactors n = map (\sub -> (length sub, head sub)) . group . primeFactorList n $ 2

primeFactorList :: (Integral a) => a -> a -> [a]
primeFactorList n p
    | p > n = []
    | n `mod` p == 0 = p : primeFactorList (n `div` p) p
    | otherwise = primeFactorList n (fromInteger . nextPrime . (1+) . toInteger $ p)