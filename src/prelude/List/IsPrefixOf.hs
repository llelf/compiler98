module List where

isPrefixOf               :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _           =  True
isPrefixOf (x:xs) (y:ys)  =  x == y && isPrefixOf xs ys

