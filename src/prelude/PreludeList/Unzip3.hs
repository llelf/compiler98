module Prelude where

unzip3    :: [(a,b,c)] -> ([a],[b],[c])
#if !defined(TRACING)
unzip3    = foldr (\(b,c,d) ~(bs,cs,ds) -> (b:bs,c:cs,d:ds)) ([],[],[])
#else
unzip3 x  = foldr (\(b,c,d) ~(bs,cs,ds) -> (b:bs,c:cs,d:ds)) ([],[],[]) x
#endif
