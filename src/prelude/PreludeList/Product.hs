module Prelude where

product :: (Num a) => [a] -> a
#if !defined(TRACING)
product = foldl (*) 1
#else
product x = foldl (*) 1 x
#endif
