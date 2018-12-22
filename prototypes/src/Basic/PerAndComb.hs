module Basic.PerAndComb where 
 
import Data.List (delete, tails)  

insert :: a -> [a] -> [[a]]
insert n [] = [[n]]
insert n (x:xs) = (n : x : xs) : [ x : ys | ys <- insert n xs]


permutation :: [a] -> [[a]]
permutation [] = [[]]
permutation (x:xs) = concat [insert x sub | sub <- permutation xs]

permutation' :: (Eq a) => [a] -> [[a]]
permutation' [] = [[]]
permutation' xs = [y : ys | y <- xs, ys <- permutation' (delete y xs)]

somePermutation :: (Eq a) => Int -> [a] -> [[a]]
somePermutation 0 _ = [[]]
somePermutation n xs = [y : ys | y <- xs, ys <- somePermutation (n - 1) (delete y xs)]

derangements :: [Int] -> [[Int]]
derangements [] = [[]]
derangements xs = [x : ys | x <- xs, ys <- derangements (delete x xs), x /= length xs]

derangements' n = map reverse (derangements [1..n])

powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = [x : sub | sub <- subsets] ++ subsets
    where subsets = powerSet xs

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [y : ys' | y:ys <- tails xs, ys' <- combinations (n - 1) ys]

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose n xs =  [y : ys' | y:ys <- tails xs, ys' <- choose (n - 1) (y:ys)]