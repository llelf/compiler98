module List where

intersect               :: (Eq a) => [a] -> [a] -> [a]
intersect xs ys         =  [x | x <- xs, x `elem` ys]
