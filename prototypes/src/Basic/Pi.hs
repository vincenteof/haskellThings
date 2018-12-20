module Basic.Pi where

pi' :: Int -> Double
pi' n = 4 * sum (series1 n) + 4 * sum (series2 n)
    where series1 n = [1 / fromIntegral k * (1/2)^k * (-1)^j | (k, j) <- zip [1, 3..n] [0..n]]
          series2 n = [1 / fromIntegral k * (1/3)^k * (-1)^j | (k, j) <- zip [1, 3..n] [0..n]]  