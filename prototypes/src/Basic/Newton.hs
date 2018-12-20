module Basic.Newton where

-- first param is num of interations, second is the num to calculate    
squareRoot :: Int -> Double -> Double
squareRoot 0 x = x
squareRoot n x = (squareRoot (n - 1) x + x / squareRoot (n - 1) x) / 2

fix :: (t -> t -> Bool) -> (t -> t) -> t -> t
fix c f x 
    | c x (f x) = x
    | otherwise = fix c f (f x)

newton :: Fractional a => a -> a -> a
newton c t = (c/t + t) / 2.0

mySqrt :: Double -> Double
mySqrt c = fix (\a b -> a - b < 0.000001) (newton c) c