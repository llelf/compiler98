module Prelude where

break :: (a -> Bool) -> [a] -> ([a], [a])
#if !defined(TRACING)
break p = span (not . p)
#else
break p x = span (not . p) x
#endif

