module Numeric(readInt) where

import Nonnull

#if !defined(TRACING)
readInt :: (Integral a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
#else
readInt :: (Integral a) => a -> (Char -> Bool) -> (Char -> Int) -> String -> [(a,String)]
#endif
readInt radix isDig digToInt s =
    [(foldl1 (\n d -> n * radix + d) (map (fromIntegral . digToInt) ds), r)
	| (ds,r) <- nonnull isDig s ]

