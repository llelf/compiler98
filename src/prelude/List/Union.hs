module List where

#if !defined(TRACING)

import UnionBy
union       :: (Eq a) => [a] -> [a] -> [a]
union        =  unionBy (==)

#else

import Difference
union                   :: (Eq a) => [a] -> [a] -> [a]
union xs ys             =  xs ++ (ys \\ xs)

#endif
