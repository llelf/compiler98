module Prelude where

sum :: (Num a) => [a] -> a
#if !defined(TRACING)
sum = foldl (+) 0
#else
sum x = foldl (+) 0 x
#endif

