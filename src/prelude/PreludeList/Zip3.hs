module Prelude where

import ZipWith3

zip3	   :: [a] -> [b] -> [c] -> [(a,b,c)] 
#if !defined(TRACING)
zip3       = zipWith3 (,,)
#else
zip3 x y z = zipWith3 (,,) x y z
#endif
