module Prelude where

unzip    :: [(a,b)] -> ([a],[b])
#if !defined(TRACING)
unzip    = foldr (\(b,c) ~(bs,cs) -> (b:bs, c:cs)) ([], [])
#else
unzip x  = foldr (\(b,c) ~(bs,cs) -> (b:bs, c:cs)) ([], []) x
#endif

