module List where

#if !defined(TRACING)

import IntersectBy

intersect    :: (Eq a) => [a] -> [a] -> [a]
intersect     =  intersectBy (==)

#else

intersect               :: (Eq a) => [a] -> [a] -> [a]
intersect xs ys         =  [x | x <- xs, x `elem` ys]

#endif
