module List where

import Difference

union                   :: (Eq a) => [a] -> [a] -> [a]
union xs ys             =  xs ++ (ys \\ xs)
