{-# LANGUAGE ParallelListComp, TransformListComp #-}

module Basic.ParallelListComprehension where 

import GHC.Exts

parallelExample = [(x,y,z) | x <- [1 ,2, 3] , y <- [4, 5, 6] | z <- [7, 8, 9]]

table = 
    [ ("Hangzhou", "MP4", 243)
    , ("Hangzhou", "CD", 925)
    , ("Beijing", "MP4", 157)
    , ("Beijing", "CD", 536)
    , ("Shanghai", "MP4", 784)
    , ("Shanghai", "CD", 766) 
    ]

analysis = [(the product, sum cost) |
                (city, product, cost) <- table,
                then group by product using groupWith,
                then sortWith by sum cost]