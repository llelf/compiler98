module Prelude where

infix 4 `elem`

elem :: (Eq a) => a -> [a] -> Bool
#if !defined(TRACING)
elem x = any (x==)
#else
elem x xs = any (x==) xs
#endif
